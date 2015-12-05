structure PP :> PP =
  struct
    val debug   = false 
    val strlen  = String.size
    val nl      = "\n"
      
    datatype gmode =
      GFlat             (* hgrp *)
    | GBreak            (* vgrp *)
    | GFill             (* fgrp *)
    | GAuto             (* agrp *)
      

    datatype doc =
      DocNil
    | DocCons           of doc * doc
    | DocText           of string
    | DocNest           of int * doc
    | DocBreak          of string
    | DocGroup          of gmode * doc

    infixr 6 ^^
    infixr 6 ^/

    fun (x ^^ y)    = DocCons(x,y)
    val empty            = DocNil
    fun text s           = DocText(s)
    fun nest i x         = DocNest(i,x)
    val break            = DocBreak(" ")
    fun breakWith s      = DocBreak(s)
    fun hgrp d           = DocGroup(GFlat, d)
    fun vgrp d           = DocGroup(GBreak,d)
    fun agrp d           = if debug
			     then DocGroup(GAuto, text "[" ^^ d ^^ text "]")
			   else DocGroup(GAuto, d)
    fun fgrp d           = if debug
			     then DocGroup(GFill, text "{" ^^ d ^^ text "}")
			   else DocGroup(GFill, d)
                          
    datatype sdoc =
      SNil
      | SText             of string * sdoc
      | SLine             of int    * sdoc    (* newline + spaces *)
      
    fun mkline i = let
      fun f 0 = Byte.charToByte #"\n"
	| f _ = Byte.charToByte #" "
    in Byte.bytesToString(Word8Vector.tabulate(i+1,f))
    end

    fun sdocToString sdoc = let
      val buf = ref []
      fun loop SNil =()
	| loop (SText(s,d)) = (buf := s::(!buf); loop d)
	| loop (SLine(i,d)) = ( buf := (mkline i)::(!buf);loop d)
    in loop sdoc; String.concat (List.rev (!buf))
    end

    fun sdocToFile oc doc = let 
      fun pstr s = TextIO.output(oc,s)
      fun loop SNil = () 
	| loop (SText(s,d)) = (pstr s; loop d)
	| loop (SLine(i,d)) = (pstr (mkline i); loop d)
    in loop doc
    end
  
    datatype mode =
      Flat
    | Break
    | Fill

    fun fits w x = 
      if (w < 0) then false
      else (case x of
	      []                             => true
	    | (i,m,DocNil)              :: z => fits w z
	    | (i,m,DocCons(x,y))        :: z => fits w ((i,m,x)::(i,m,y)::z)
	    | (i,m,DocNest(j,x))        :: z => fits w ((i+j,m,x)::z)
	    | (i,m,DocText(s))          :: z => fits (w - strlen s) z
	    | (i,Flat, DocBreak(s))     :: z => fits (w - strlen s) z
	    | (i,Fill, DocBreak(_))     :: z => true 
	    | (i,Break,DocBreak(_))     :: z => true
	    | (i,m,DocGroup(_,x))       :: z => fits w ((i,Flat,x)::z))

    fun format w k x = 
      (case x of 
	 []                            => SNil
      | (i,m,DocNil)              :: z => format w k z
      | (i,m,DocCons(x,y))        :: z => format w k ((i,m,x)::(i,m,y)::z)
      | (i,m,DocNest(j,x))        :: z => format w k ((i+j,m,x)::z)
      | (i,m,DocText(s))          :: z => SText(s ,format w (k + strlen s) z)
      | (i,Flat, DocBreak(s))     :: z => SText(s ,format w (k + strlen s) z)
      | (i,Fill, DocBreak(s))     :: z => let val l = strlen s in
	                                  if   fits (w - k - l) z 
					    then SText(s, format w (k+l) z)
					  else SLine(i, format w  i    z)
                                          end
      | (i,Break,DocBreak(s))     :: z => SLine(i,format w i z)
      | (i,m,DocGroup(GFlat ,x))  :: z => format w k ((i,Flat ,x)::z)
      | (i,m,DocGroup(GFill ,x))  :: z => format w k ((i,Fill ,x)::z)
      | (i,m,DocGroup(GBreak,x))  :: z => format w k ((i,Break,x)::z)
      | (i,m,DocGroup(GAuto, x))  :: z => if fits (w-k) ((i,Flat,x)::z)
					    then format w k ((i,Flat ,x)::z)
					  else format w k ((i,Break,x)::z))
					    

    fun ppToString w doc =
      sdocToString (format w 0 [(0,Flat,agrp(doc))])
      
    fun ppToFile oc w doc =
      sdocToFile oc (format w 0 [(0,Flat,agrp(doc))])
      
    fun list sep f xs = let 
      fun loop (acc,[])  = acc
	| loop (acc,[x]) = acc ^^ f x 
	| loop (acc,(x::xs)) = loop ((acc ^^ f x ^^ sep),xs)
    in loop (empty,xs)
    end
  
    fun commalist f = list (text "," ^^ break) f
      
    fun (x ^/ y)   = x ^^ break ^^ y 
  end
