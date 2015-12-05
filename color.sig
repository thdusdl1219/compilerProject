signature COLOR = sig

   type coloring = {alloc: Mips.allocation,
	            spills: Mips.RegSet.set}

   val color : {interference: Liveness.IG.graph,
	        moves: Liveness.IG.graph,
                spillCost: Mips.reg -> int,
                palette: Mips.RegSet.set} -> 
               coloring

   val verify: {complain: string->unit,
                func: Mips.funcode,
                spillCost: Mips.reg -> int,
                palette: Mips.RegSet.set,
                coloring: coloring}
                -> unit

   val spillCostInfinity : int (* Don't spill a register that has this cost! *)

end
