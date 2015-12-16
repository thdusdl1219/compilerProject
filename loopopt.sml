signature LOOPOPT = 
sig
	val optimize: X86.funcode -> X86.funcode
end

structure LoopOpt : LOOPOPT = struct
	structure M = X86
	structure RS = M.RegSet

fun compareBlock (x,y) = 
	let val (lab1,insts1) = x
		val (lab2,insts2) = y
	in
		Symbol.compare (lab1, lab2)
	end

fun isEqualBlock (x,y) = 
	case compareBlock(x,y) of
		EQUAL => true
		| _ => false

fun print_set (set:RS.set) : unit = 
	(RS.app (fn reg => (print " "; print (M.reg2name reg))) set; print "\n")

structure BBSet = RedBlackSetFn(type ord_key = M.codeblock
	val compare = compareBlock)

fun list2set l = BBSet.addList(BBSet.empty,l)

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
				| M.Branch(_,_,_,lab) => (addSucc (labToBlock funCode lab); addSucc (nextBlock funCode thisLab))
				| _ => addSucc (nextBlock funCode thisLab)
			end
		val _ = List.map (calcSucc) funCode
	in 
		()
	end


(* DomMap *)
structure DomMap = RedBlackMapFn( struct type ord_key = M.codeblock val compare = compareBlock end)

fun bbset2string (set:BBSet.set) =
			BBSet.foldl (fn ((lab,instrs),str) => (str^" "^(Symbol.name lab))) "" set

fun bb2string (block:M.codeblock) =
	let val (lab, _) = block
	in
		Symbol.name lab
	end

fun dumpDomMap map =
	let
		val blockList = DomMap.listItemsi map
		
		fun dumpEntries ((key,set)::t) = 
			let 
				val (lab, instrs) = key
			in
				print (Symbol.name lab^" <= "^bbset2string set^"\n");
				dumpEntries t
			end
			| dumpEntries [] = ()
	in
		dumpEntries blockList
	end

fun isEqualDomMap (map1, map2) =
	let
		fun compareOneInTheSecond (key, set)=
			case DomMap.find(map2, key) of
				NONE => false
				| SOME s => BBSet.equal( s, set )
		val list1 = DomMap.listItemsi map1

		fun compareAllInTheFirst (h::t) = (compareOneInTheSecond h) andalso compareAllInTheFirst t
			| compareAllInTheFirst [] = true
	in
		compareAllInTheFirst list1
	end

fun makeDomMap cfg entry =
	let 
		val initialDomMap = BBSet.foldl (fn (b,m) => DomMap.insert(m, b, CFG.nodes cfg)) DomMap.empty (CFG.nodes cfg)
		fun updateDom oldmap (block, map) =
			let
				val (lab, _) = block 
				(*val _ = print ("updating :"^Symbol.name lab^"\n" )*)
				fun dom block = 
					if isEqualBlock (block, entry) then BBSet.singleton(block)
					else 
						case DomMap.find( oldmap, block ) of
							NONE => BBSet.empty
							| SOME v => v

				fun intersect (block, set) = 
					BBSet.intersection( set, dom block )
			in
				if isEqualBlock (block, entry) 
				then 
					DomMap.insert(map, block, BBSet.singleton(block))
				else
					let 
						(*val _ = print ("predecessors: "^bbset2string (CFG.pred cfg block)^"\n")*)
						val intersected = BBSet.foldl intersect (CFG.nodes cfg) (CFG.pred cfg block)
						(*val _ = print ("bbset is:"^bbset2string intersected^"\n")*)
					in
						DomMap.insert( map, block, BBSet.union(intersected, BBSet.singleton(block)))
					end
			end
		fun updateDomAll oldDomMap = 
			let
				val newDomMap = BBSet.foldl (updateDom oldDomMap) DomMap.empty (CFG.nodes cfg) 
			in
				if isEqualDomMap(oldDomMap, newDomMap) 
				then
					(*(print "\n *** DOMMAP: \n"; dumpDomMap newDomMap;newDomMap)*)newDomMap
				else
					(*(print ("notEqual\n\n\n\n\n\n");*)
					updateDomAll newDomMap
			end
	in
		updateDomAll initialDomMap
	end

fun printsucc say g (i,insts) = 
   (say (Symbol.name i); say ":";
    CFG.S.app (fn (j,_) => (say " "; say (Symbol.name j))) (CFG.succ g (i,insts));
    say "\n")


fun printgraph say g = CFG.S.app (printsucc say g) (CFG.nodes g);

fun findLoops cfg map =
	let 
		fun doDFS (to:M.codeblock) (from:M.codeblock, set:BBSet.set):BBSet.set =
			let
				(*val _ = print ("doDFS in "^bb2string from^"\n")*)
				val (fromLab, _) = from
				val (toLab, _) = to
			in
				if fromLab = toLab
				then
					BBSet.add(set,from)
				else
					BBSet.union(BBSet.foldl (doDFS to) (BBSet.empty) (CFG.pred cfg from), BBSet.add (set, from) )
			end

		fun findloop (block:M.codeblock,looplist) =
			let 
				(*val _ = print (bb2string block)*)
				val (lab,_) = block
				val SOME dominators = DomMap.find( map, block )
				val backto = BBSet.intersection(dominators , (CFG.succ cfg block))
				val body = doDFS 
				(*val _ = print (" header: "^bbset2string backto^"\n")*)
			in
				case BBSet.numItems backto of
					0 => looplist
					| 1 => let
						val [header] = BBSet.listItems backto
						val body = doDFS header (block, BBSet.empty)
						val _ = print ("*** Found Loop : header("^bb2string header^") and body("^bbset2string body^")\n")
					in
						(header,body)::looplist
					end
					| _ => ErrorMsg.impossible "'Double back edge from one node' case is not implemented!"
			end
		

	in
		BBSet.foldl (findloop) [] (CFG.nodes cfg)
	end


fun defsInBody (body:BBSet.set):RS.set =
	let
		fun defsInInstruction (instruction, set:RS.set) =
			RS.union( set, #def (M.instr_def_use instruction))

		fun defsInBlock (block:M.codeblock, set:RS.set):RS.set =
			let val (lab, instrs) = block
			in
				foldl (defsInInstruction) set instrs
			end
	in
		BBSet.foldl (defsInBlock) RS.empty body
	end



fun invariantsInBody (alldefs:RS.set) (invariants:RS.set) (body:BBSet.set):RS.set =
	let 
		fun isInvariant (instruction:M.instruction) =
			let
				fun useRegInvariant (r:M.reg, isit:bool) = 
					(isit andalso (RS.member(invariants,r) orelse (not (RS.member(alldefs,r)))))

				val uses = #use (M.instr_def_use instruction)
				val isUseRegsInvariant = RS.foldl useRegInvariant true uses
			in
				case instruction of
					M.Arith1 (aop1,_) => isUseRegsInvariant
				    | M.Arith2 (aop2,_,_) => isUseRegsInvariant
				    | M.Arith3 (aop3,_,_,_) => isUseRegsInvariant
				    | M.Arithi (aopi,_,_, immed) => isUseRegsInvariant
				    | M.Li (_, immed) => true
				    | M.La (_, lab) => true
				    | M.Lw (_, address) => false
				    | M.Sw (_, address) => false
				    | M.Move (_,_) => isUseRegsInvariant
				    | M.Branchz (comparecode1,_, lab) => false
				    | M.Branch (comparecode1,_,_, lab) => false
				    | M.J (lab) => false
				    | M.Jal (lab) => false
				    | M.Jr (_,_) => false
				    | M.Jalr (_,_, _, _) => false
				    | M.Nop => false
				    | M.Syscall => false
				    | M.Leave => false
				    | M.Ret => false
				    | M.Push (_) => false
				    | M.Pop (_) => false
				    | M.Branch2 (comparecode1, lab) => false
			end
		fun invariantsInInstruction (instruction, set:RS.set) = 
			let val defs = #def (M.instr_def_use instruction)
			in
				if (isInvariant instruction)
				then
					RS.union(set, defs)
				else 
					set
			end

		fun invariantsInBlock ((lab, instrs), set:RS.set) =
			foldl (invariantsInInstruction) set instrs


		val oldNumInvariants = RS.numItems invariants
		val newInvariants = BBSet.foldl invariantsInBlock invariants body
		val newNumInvariants = RS.numItems newInvariants
	in 
		if (oldNumInvariants = newNumInvariants)
		then
			newInvariants
		else
			invariantsInBody alldefs newInvariants body
	end
(*
fun filterConstraints (invariants:RS.set) (body:BBSet.set):RS.set =
	let
		fun filterInBlock (block:M.codeblock,filtered) =

	in
		body
	end
*)

fun singleDefsInBody (body:BBSet.set):RS.set =
	let
		fun singleDefsInInstruction (instruction, (set:RS.set,mset:RS.set)):RS.set * RS.set =
			let val defs = #def (M.instr_def_use instruction)
				(*val _ = print (M.instr2string instruction)
				val _ =  print_set defs
				val _ = print "set is "
				val _ =  print_set (RS.union(set,defs))
				val _ = print "mset is "
				val _ =  print_set (RS.union(RS.intersection(defs, set) , mset) )*)
			in
				(RS.union(set,defs), ( RS.union(RS.intersection(defs, set) , mset) ))
			end

		fun singleDefsInBlock (block:M.codeblock, (set:RS.set,mset:RS.set)):RS.set *RS.set =
			let val (lab, instrs) = block
			in
				foldl (singleDefsInInstruction) (set, mset) instrs
			end
	in
		let val (set,mset) = BBSet.foldl (singleDefsInBlock) (RS.empty, RS.empty) body
		in
			RS.difference(set, mset)
		end
	end

fun optimize (funCode:M.funcode) : M.funcode =
	let 
		fun optimizeLoop (header:M.codeblock, body:BBSet.set) =
			let
				val defset = defsInBody (body)
				val singledefs = singleDefsInBody (body)
				val _ = print "invariants: "
				val invariants = RS.intersection( invariantsInBody defset RS.empty (body), singledefs)
				val _ = print_set invariants
			in
				(header,body)
			end

		fun optimizeLoops (h::t) = 
			(optimizeLoop h)::(optimizeLoops t)
			| optimizeLoops [] = []

		val g = CFG.newGraph()
		val _ = makeCFG g funCode
		val domMap = makeDomMap g (hd(funCode))
		val loops = findLoops g domMap

		val _ = optimizeLoops loops
		(*val _ = print ("###### DomMap\n")
		val _ = dumpDomMap domMap*)
	in
		(*printgraph print g;
		print ("###### END \n");*)
		funCode
	end

end