#!/usr/bin/python

import argparse
import re
import os
import subprocess
import shlex
import time

class Machine:
    def load(self, filename):
        outname = filename.split(".")[0]
        exe = "gcc -o " + outname + " " + filename
        exe = shlex.split(exe)
        subprocess.Popen(exe)

    def start(self, filename):
        outname = filename.split(".")[0]
        exe = outname
        exe = shlex.split(exe)
        excute_instance = subprocess.Popen(exe, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        (excute_stdout, excute_stderr) = excute_instance.communicate()
        return excute_stdout



pass_test = 0
fail_test = 0


def run_test(filename):
    global pass_test
    global fail_test

    if use_allocated:
        assembly_filename = filename + ".s"
    else:
        assembly_filename = filename + ".noregalloc.s"
    stdout_filename = filename + ".stdout"
    output_filename = filename + ".out"

    if is_check:
        if which(output_filename, os.R_OK) is not None:
            print "A fun source code \"" + filename + "\" and the answer file \"" + output_filename + "\" is found. Testing.."
        else:
            print "A fun source code \"" + filename + "\" is found but its answer file \"" + output_filename + "\" does not exist. Skipping.."
            return;
    else:
        print "A fun source code \"" + filename + "\" is found. Running.."

    if os.path.isfile(assembly_filename):
        os.remove(assembly_filename)
    elif os.path.isdir(assembly_filename):
        os.removedirs(assembly_filename)
    
    compiler_instance = subprocess.Popen([sml, '-m', sources], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (compiler_stdout, compiler_stderr) = compiler_instance.communicate(input="Compile.compile(\"" + filename + "\"); OS.Process.exit(OS.Process.success) ;")

    returncode = compiler_instance.returncode
    if returncode != 0:
        print "Error in checking sml\nError code : %d" % returncode
        print "output from sml :\n" + sml_stdout
        print "error from sml :\n" + sml_stderr
        return;

    if not which(assembly_filename, os.R_OK):
        print "Compile failure"
        print "output from compiler :\n" + compiler_stdout
        print "error from compiler :\n" + compiler_stderr
        return;

    # Print the compile output
    with open(stdout_filename, "wt") as stdout_file:
        print >> stdout_file, compiler_stdout

    m = Machine();
    m.load(assembly_filename)
    time.sleep(0.5)
    if is_output:
        with open(output_filename, "wt") as output_file:
            print >> output_file, m.start(assembly_filename)
    elif is_check:
        with open(output_filename, "rt") as output_file:
            re_list = re.compile(r"^\s*\[\s*(?:None|-?\s*\d+)(?:\s*,\s*(?:None|-?\s*\d+))*\s*\]\s*$")
            matched_list = None

            for line in output_file:
                if re_list.match(line) is not None:
                    matched_list = line.strip()
                    break

            if matched_list is None:
                print "The answer file does not have an answer. Skipping.."
                return;

            ans = eval(matched_list)[:-1]
            cur = m.start(assembly_filename)
            cur = cur.split("\n")[:-1]
            cur = map(lambda x: int(x), cur)

            if ans == cur:
                print "PASS"
                pass_test = pass_test + 1
            else:
                print "FAIL"
                print "Answer :\t" + ", ".join(map(str,ans)) + "(return value)"
                print "Output :\t" + ", ".join(map(str,cur)) + "(return value)"
                fail_test = fail_test + 1
    else:
        m.start(assembly_filename)

def which(spath, cond):
    def cond_check(fpath):
        return os.path.isfile(fpath) and os.access(fpath, cond)

    fpath, fname = os.path.split(spath)
    if fpath:
        if cond_check(spath):
             return spath
    else:
        path_candidate = [os.getcwd()] + (os.environ["PATH"].split(os.pathsep))
        for path in path_candidate:
            path = path.strip('"')
            full_path = os.path.join(path, spath)
            if cond_check(full_path):
                return full_path
    return None

def check_sml(sml, sources):
    sml_instance = subprocess.Popen([sml, '-m', sources], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (sml_stdout, sml_stderr) = sml_instance.communicate(input="OS.Process.exit(OS.Process.success) ;")

    returncode = sml_instance.returncode
    if returncode != 0:
        print "Error in checking sml\nError code : %d" % returncode
        print "output from sml :\n" + sml_stdout
        print "error from sml :\n" + sml_stderr
        sys.exit(returncode)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='script for test')

    default_sml = "sml"
    default_sources = "sources.cm"

    parser.add_argument('testfiles', metavar='testfiles', nargs='+', help='a file name or a directory name for the test file')
    parser.add_argument("-s", "--sml", metavar='sml', help="A command for invoking sml interpreter. (default : \"" + default_sml + "\")", default=default_sml)
    parser.add_argument("-S", "--sources", metavar='sources', help="A path for sources.cm file. (default : \"" + default_sources + "\")", default=default_sources)
    group = parser.add_mutually_exclusive_group()
    group.add_argument("-c", "--check", help="checks your implementation with answer files", action="store_true")
    group.add_argument("-o", "--output", help="generates answer files", action="store_true")
    parser.add_argument("-a", "--allocated", help="uses *.s instead of *.noregalloc.s", action="store_true")

    args = parser.parse_args()

    global sml
    sml = which(args.sml, os.X_OK)
    if sml is None:
        print "The executable file \"" + args.sml + "\" is not found"
        sys.exit(-1)

    global sources
    sources = which(args.sources, os.R_OK)
    if sources is None:
        print "The readable file \"" + args.sources + "\" is not found"
        sys.exit(-1)

    check_sml(sml, sources)

    global is_check
    is_check = args.check
    global is_output
    is_output = args.output
    global use_allocated
    use_allocated = args.allocated

    re_fun = re.compile('^.*\.fun$')

    for testfile in args.testfiles:
        abs_testfile = which(testfile, os.R_OK)
        if abs_testfile is not None:
            run_test(abs_testfile)
        elif os.path.isdir(testfile):
            for root, dirs, files in os.walk(testfile):
                for filename in files:
                    if re_fun.match(filename):
                        abs_testfile = which(os.path.join(root, filename), os.R_OK)
                        if abs_testfile is not None:
                            run_test(abs_testfile)
        else:
            print "The testfile \"" + testfile + "\" is not found. Skipping.."

    if is_check:
        print "%d test(s), %d pass(es), %d fail(s)" % (pass_test + fail_test, pass_test, fail_test)
