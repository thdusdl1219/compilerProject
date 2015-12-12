signature COLOR = sig

   type coloring = {alloc: X86.allocation,
	            spills: X86.RegSet.set}

   val color : {interference: Liveness.IG.graph,
	        moves: Liveness.IG.graph,
                spillCost: X86.reg -> int,
                palette: X86.RegSet.set} -> 
               coloring

   val verify: {complain: string->unit,
                func: X86.funcode,
                spillCost: X86.reg -> int,
                palette: X86.RegSet.set,
                coloring: coloring}
                -> unit

   val spillCostInfinity : int (* Don't spill a register that has this cost! *)

end
