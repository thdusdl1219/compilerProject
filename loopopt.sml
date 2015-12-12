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

fun isEqualBlock (x,y) = 
	case compareBlock(x,y) of
		EQUAL => true
		| _ => false

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
				val _ = print ("updating :"^Symbol.name lab^"\n" )
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
						val _ = print ("predecessors: "^bbset2string (CFG.pred cfg block)^"\n")
						val intersected = BBSet.foldl intersect (CFG.nodes cfg) (CFG.pred cfg block)
						val _ = print ("bbset is:"^bbset2string intersected^"\n")
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
					(print "\n *** DOMMAP: \n"; dumpDomMap newDomMap;newDomMap)
				else
					(print ("notEqual\n\n\n\n\n\n");
					updateDomAll newDomMap)
			end
	in
		updateDomAll initialDomMap
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
		val domMap = makeDomMap g (hd(funCode))
		val _ = print ("###### DomMap\n")
		val _ = dumpDomMap domMap
	in
		printgraph print g;
		print ("###### END \n");
		funCode
	end

end