#!/usr/bin/python

# This emulator was originally written by Hanhwi jang.

import argparse
import subprocess
import os
import re
import sys

class Inst:
    def __init__ (self, text):
        re_instruction = re.compile(r"""
            ^
            (?P<op>\w+)                                 # Opcode
            (?:
                $ |                                     # No operand (e.g., syscall, nop)
                \s+(?P<label_1>[A-Za-z_]\w+)$ |         # One label operand (e.g., j, jal)
                \s+(?P<r1>\$\w+)                        # The first register operand
                (?:
                    $ |                                 # One register operand (e.g., jr)
                    ,\s+(?P<label_2>[A-Za-z_]\w+)$ |    # One register and one label operands (e.g., la, branchz)
                    ,\s+(?P<immed_1>-?\d+)              # An immediate operand
                    (?:
                        $ |                             # One register and one immediate operands (e.g., li)
                        \((?P<r2_1>\$\w+)\)$            # One register and one address operands (e.g., lw, sw)
                    ) |
                    ,\s+(?P<r2_2>\$\w+)                 # The second register operand
                    (?:
                        $ |                             # Two registers operand (e.g., Move)
                        ,\s+(?P<label_3>[A-Za-z_]\w+)$ |# Two registers and one label operands (e.g., branchu, branch)
                        ,\s+(?P<immed_2>-?\d+)$ |       # Two registers and one immediate operands (e.g., addi)
                        ,\s+(?P<r3>\$\w+)$              # Three registers operands (e.g., add)
                    )
                )
            )
        """, re.VERBOSE)
        m = re_instruction.match(text)
        if m is not None:
            self.op = m.group('op')
            self.r1 = m.group('r1')
            if m.group('r2_1') is not None:
              self.r2 = m.group('r2_1')
            else:
              self.r2 = m.group('r2_2')
            self.r3 = m.group('r3')
            if m.group('immed_1') is not None:
              self.immed = m.group('immed_1')
            else:
              self.immed = m.group('immed_2')
            if self.immed is not None:
              self.immed = int(self.immed)
            if m.group('label_1') is not None:
              self.label = m.group('label_1')
            elif m.group('label_2') is not None:
              self.label = m.group('label_2')
            else:
              self.label = m.group('label_3')
        else:
            print "ERROR [%s]" % text

    def _print(self):
        print self.op, self.r1, self.r2, self.r3, self.immed, self.label

class RF:
    archRegsName =  [ "$zero", "$at", "$v0", "$v1","$a0","$a1" ,"$a2", "$a3",
                      "$t0" ,"$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
                      "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
                      "$t8", "$t9", "$k0", "$k1", "$gp", "$sp", "$fp", "$ra"]
    archRegsMap = dict(zip(archRegsName, range(0, len(archRegsName))))

    def getReg(self, reg_name):
        if reg_name in RF.archRegsMap:
            return self.archRegsVal[RF.archRegsMap[reg_name]]
        else:
            reg_index = int(reg_name[2:])
            if reg_index in self.virtRegsStack[-1]:
                return self.virtRegsStack[-1][reg_index]
            else:
                return None
    def setReg(self, reg_name, value):
        if reg_name in RF.archRegsMap:
            if reg_name != "$zero":
                self.archRegsVal[RF.archRegsMap[reg_name]] = value
        else:
            reg_index = int(reg_name[2:])
            self.virtRegsStack[-1][reg_index] = value
    def push(self):
        self.virtRegsStack.append({})
    def pop(self):
        del self.virtRegsStack[-1]

    def __init__ (self):
        self.archRegsVal = [None for x in xrange(0, len(RF.archRegsName))]

        self.archRegsVal[0] = 0
        self.setReg('$sp', 0xc0000000)
        self.setReg('$gp', 0xdeadbeef)
        self.setReg('$ra', -1)

        self.virtRegsStack = [{}]
        return

class Machine:
    def get_block_idx(self, addr):
        return addr / 4

    def syscall(self):
        if self.rf.getReg("$v0") == 9:
            # sbrk
            self.rf.setReg("$v0", 0xffff0000)
        return

    def execute(self):
        # Arit2 case
        r = 0
        dst = None
        nextip = self.ip + 1
        inst = self.insts[self.ip]
        # print "IP", self.ip
        # inst._print()
        if inst.op == 'abs':
            r = abs(self.rf.getReg(inst.r2))
            dst = inst.r1
        elif inst.op == 'neg':
            r = -(self.rf.getReg(inst.r2))
            dst = inst.r1
        elif inst.op == 'not':
            if self.rf.getReg(inst.r2) == 0:
                r = 1
            else:
                r = 0
            dst = inst.r1
        elif inst.op == 'add':
        # Arith3 case
            r = (self.rf.getReg(inst.r2) + self.rf.getReg(inst.r3))
            dst = inst.r1
        elif inst.op == 'and':
            r = (self.rf.getReg(inst.r2) & self.rf.getReg(inst.r3))
            dst = inst.r1
        elif inst.op == 'mulo':
            r = (self.rf.getReg(inst.r2) * self.rf.getReg(inst.r3))
            dst = inst.r1
        elif inst.op == 'div':
            r = (self.rf.getReg(inst.r2) / self.rf.getReg(inst.r3))
            dst = inst.r1
        elif inst.op == 'or':
            r = (self.rf.getReg(inst.r2) | self.rf.getReg(inst.r3))
            dst = inst.r1
        elif inst.op == 'rem':
            r = (self.rf.getReg(inst.r2) % self.rf.getReg(inst.r3))
            dst = inst.r1
        elif inst.op == 'sub':
            r = (self.rf.getReg(inst.r2) - self.rf.getReg(inst.r3))
            dst = inst.r1
        elif inst.op == 'xor':
            r = (self.rf.getReg(inst.r2) ^ self.rf.getReg(inst.r3))
            dst = inst.r1
        elif inst.op == 'seq':
            if self.rf.getReg(inst.r2) == self.rf.getReg(inst.r3):
                r = 1
            else:
                r = 0
            dst = inst.r1
        elif inst.op == 'slt':
            if self.rf.getReg(inst.r2) < self.rf.getReg(inst.r3):
                r = 1
            else:
                r = 0
            r = (self.rf.getReg(inst.r2) < self.rf.getReg(inst.r3))
            dst = inst.r1
        # Arithi case
        elif inst.op == 'addi':
            r = (self.rf.getReg(inst.r2) + inst.immed)
            dst = inst.r1
        elif inst.op == 'andi':
            r = (self.rf.getReg(inst.r2) & inst.immed)
            dst = inst.r1
        elif inst.op == 'orii':
            r = (self.rf.getReg(inst.r2) | inst.immed)
            dst = inst.r1
        elif inst.op == 'xori':
            r = (self.rf.getReg(inst.r2) ^ inst.immed)
            dst = inst.r1
        # Li
        elif inst.op == 'li':
            r = inst.immed
            dst = inst.r1
        # La
        elif inst.op == 'la':
            r = self.inst_label[inst.label]
            dst = inst.r1
        # Lw
        elif inst.op == 'lw':
            addr = self.rf.getReg(inst.r2) + inst.immed
            r = self.data[self.get_block_idx(addr)]
            dst = inst.r1
        # Sw
        elif inst.op == 'sw':
            addr = self.rf.getReg(inst.r2) + inst.immed
            self.data[self.get_block_idx(addr)] = self.rf.getReg(inst.r1)
            dst = None
        # Move
        elif inst.op == 'move':
            r = self.rf.getReg(inst.r2)
            dst = inst.r1
        # Branchz
        elif inst.op == 'bltz':
            if self.rf.getReg(inst.r1) < 0:
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'beqz':
            if self.rf.getReg(inst.r1) == 0:
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'bnez':
            if self.rf.getReg(inst.r1) != 0:
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'bgez':
            if self.rf.getReg(inst.r1) >= 0:
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'bgtz':
            if self.rf.getReg(inst.r1) > 0:
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'blez':
            if self.rf.getReg(inst.r1) <= 0:
                nextip = self.inst_label[inst.label]
            dst = None
        # Branchu (* Not correctly implemented *)
        elif inst.op == 'bltu':
            if self.rf.getReg(inst.r1) < self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'bgeu':
            if self.rf.getReg(inst.r1) >= self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'bgtu':
            if self.rf.getReg(inst.r1) > self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'bleu':
            if self.rf.getReg(inst.r1) <= self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'blt':
            if self.rf.getReg(inst.r1) < self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
        # Branch
        elif inst.op == 'beq':
            if self.rf.getReg(inst.r1) == self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'bne':
            if self.rf.getReg(inst.r1) != self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'bge':
            if self.rf.getReg(inst.r1) >= self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'bgt':
            if self.rf.getReg(inst.r1) > self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
        elif inst.op == 'ble':
            if self.rf.getReg(inst.r1) <= self.rf.getReg(inst.r2):
                nextip = self.inst_label[inst.label]
            dst = None
            pass
        elif inst.op == 'j':
            nextip = self.inst_label[inst.label]
            dst = None
            pass
        elif inst.op == 'jal':
            r = nextip
            if inst.label == '_printint':
                if (not is_check) and (not is_output):
                    print "PRINT:", self.rf.getReg("$a0")
                else:
                    self.output_list.append(self.rf.getReg("$a0"))
            else:
                nextip = self.inst_label[inst.label]
                self.rf.push()
            dst = "$ra"
        elif inst.op == 'jr':
            if inst.r1 == '$ra':
                self.rf.pop()
            nextip = self.rf.getReg(inst.r1)
            dst = None
            pass
        elif inst.op == 'jalr':
            if self.rf.getReg(inst.r2) == self.inst_label['_printint']:
                if (not is_check) and (not is_output):
                    print "PRINT:", self.rf.getReg("$a0")
                else:
                    self.output_list.append(self.rf.getReg("$a0"))
            else:
                r = nextip
                nextip = self.rf.getReg(inst.r2)
                self.rf.push()
                dst = inst.r1
            pass
        elif inst.op == 'syscall':
            self.syscall()
            pass
        elif inst.op == 'nop':
            pass
        # update context
        self.ip = nextip
        if dst is not None:
#            inst._print()
#            print self.ip
#            print dst
#            print len(self.regs)
            self.rf.setReg(dst, r)
        return

    def __init__(self):
        self.rf = RF()

        self.insts = []
        self.data = {} # data

        # Instruction Pointer
        self.ip = 0
        self.inst_label = {}
        self.data_base = 0
        self.static_base = 0x10000000

        if is_check or is_output:
            self.output_list = []

        return

    def load(self, fname):
        with open(fname, 'rt') as f: 
            re_label = re.compile('([A-Za-z_.0-9]+):')
            is_text = False
            max_reg = 0
            for line in f:
                i_comment_begin = line.find('#')
                if i_comment_begin == -1:
                  # This line does not have a comment; just stripping spaces
                  l = line.strip()
                elif i_comment_begin == 0:
                  # The whole line is a comment
                  continue
                else:
                  # Removing comments and stripping spaces
                  l = (line[0:i_comment_begin]).strip()

                if l == "":
                    continue
                elif l == '.data':
                    text_flag = False
                    continue
                elif l == '.text':
                    text_flag = True
                    continue
                elif text_flag is False:
                    # data mode
                    # label ?
                    # data ?
                    # Ignore everything; fun program does not need a data section
                    continue
                else:
                    # text mode
                    # label ?
                    m = re_label.match(l)
                    if m is not None:
                        self.inst_label[m.group(1)] = self.ip
                    else:
                        i = Inst(l)
                        self.insts.append(i)
                        self.ip = self.ip + 1
        return
        
    def start(self):
        self.ip = self.inst_label['main']

        while True:
            self.execute ()
            if self.ip == -1:
                break
            else:
                pass

        if (not is_check) and (not is_output):
            print "main function returns %s" % self.rf.getReg('$v0')
            return;
        else:
            self.output_list.append(self.rf.getReg("$v0"))
            return self.output_list
        #print self.regs

    def stat (self):
        return

# I've got this function from http://stackoverflow.com/questions/377017/test-if-executable-exists-in-python
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

    if is_output:
        with open(output_filename, "wt") as output_file:
            print >> output_file, m.start()
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

            ans = eval(matched_list)
            cur = m.start()

            if ans == cur:
                print "PASS"
                pass_test = pass_test + 1
            else:
                print "FAIL"
                print "Answer :\t" + ", ".join(map(str,ans)) + "(return value)"
                print "Output :\t" + ", ".join(map(str,cur)) + "(return value)"
                fail_test = fail_test + 1
    else:
        m.start()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Emulate a mips processor for a program with virtual registers')

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
