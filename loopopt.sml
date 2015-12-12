signature LOOPOPT = 
sig
	val optimize: Mips.funcode -> Mips.funcode
end

structure LoopOpt : LOOPOPT = struct
	structure M = Mips

fun compareBlock (x,y) = 
	let val (lab1,insts1) = x
		val (lab2,insts2) = y
	in
		Symbol.compare (lab1, lab2)
	end
structure BBSet = RedBlackSetFn(type ord_key = M.codeblock
	val compare = compareBlock)

structure CFG = Graph(BBSet)



fun labToBlock (h::t) lab =
	let val (l,instrs) = h in
		if (l = lab) then h else labToBlock t lab
	end
	| labToBlock [] lab = ErrorMsg.impossible "No label found"


fun nextLabel funCode (lab:M.lab) =
      let fun first_label_of ((ret, _)::t) = (ret) | first_label_of [] = Symbol.symbol "#exit" in 
      case funCode of
        (lab1, _)::t => if (lab1 = lab) then first_label_of t else nextLabel t lab
        | [] => Symbol.symbol "#exit"
      end

fun nextBlock funCode (lab:M.lab) =
	let val nextlab = (nextLabel funCode lab)
	in
		if Symbol.name nextlab = "#exit" then (nextlab, []) 
		else	
			labToBlock funCode (nextLabel funCode lab)
	end

exception EmptyBlock
fun exitOf (lab, h::t) = hd(rev(h::t))
	| exitOf (lab, []) = raise EmptyBlock



fun makeCFG cfg funCode =
	let
		val _ = List.map (fn x => (CFG.succ cfg x;x)) (funCode)
		fun calcSucc block =
			let 
				fun addSucc to =
					CFG.mk_edge cfg {from=block, to=to}
				val lastinst = exitOf block 
				val (thisLab, thisInstrs) = block
			in
				case lastinst of
				M.J(lab) => addSucc (labToBlock funCode lab)
				| M.Branchz(_,_,lab) => (addSucc (labToBlock funCode lab); addSucc (nextBlock funCode thisLab))
				| _ => addSucc (nextBlock funCode thisLab)
			end
		val _ = List.map (calcSucc) funCode
	in 
		()
	end


fun printsucc say g (i,insts) = 
   (say (Symbol.name i); say ":";
    CFG.S.app (fn (j,_) => (say " "; say (Symbol.name j))) (CFG.succ g (i,insts));
    say "\n")

fun printgraph say g = CFG.S.app (printsucc say g) (CFG.nodes g);

fun optimize (funCode:M.funcode) : M.funcode =
	let 
		val g = CFG.newGraph()
		val _ = makeCFG g funCode
	in
		printgraph print g;
		funCode
	end

end