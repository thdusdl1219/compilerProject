signature ERRORMSG =
sig
    val reset : string -> unit  (* Start at the beginning of the file whose name
                                   is given by the string argument.  This does not
				   open the file, just remembers the name for
			           use in error messages *)
    val newLine : int -> unit   (* Inform the ErrorMsg module that there is a newline
			           n characters from the beginning of the file *)
    type pos = int              (* A "pos" indicates the beginning or the end of 
                                   a region of a file for which an error is indicated.
			           The beginning and end are each in distance from
				   the beginning of the file, in characters. *)
    type pos2 = pos*pos         (* the beginning and the end of a region *)
    val error : pos2 * string -> unit
				(* error(n,s) reports the error "s" at location 
				   n characters from the beginning of the file.  It
	                           uses the previously reported newLine(k) information
			           to indicate what line the error is in.  *)
    exception Error
    val impossible : string -> 'a   (* raises Error *)
    val anyErrors : bool ref
end

structure ErrorMsg : ERRORMSG =
struct

  type pos = int
  type pos2 = pos*pos
  val anyErrors = ref false
  val fileName = ref ""
  val lineNum = ref 1
  val linePos = ref [1]
  val sourceStream = ref TextIO.stdIn

  fun newLine p =  (lineNum := !lineNum+1; linePos := p :: !linePos)

  fun reset filename = (anyErrors:=false;
		 fileName:= filename;
		 lineNum:=1;
		 linePos:=[1];
		 sourceStream:=TextIO.stdIn)

  exception Error

  fun error ((first,last), msg:string) =
      let fun look(count, a::rest, n) =
		if a<count 
	        then app print [Int.toString n, ".", Int.toString (count-a)]
	        else look(count, rest, n-1)
	    | look _ = print "0.0"
       in anyErrors := true;
	  print (!fileName);
	  print ":";
	  look(first, !linePos,!lineNum);
          if last<>first then (print "-"; look(last, !linePos, !lineNum)) else ();
	  print ":";
	  print msg;
	  print "\n"
      end

  fun impossible msg =
      (app print ["Error: Compiler bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)

end  (* structure ErrorMsg *)