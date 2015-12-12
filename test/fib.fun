fun fibrec(n:int):int = if n<2 then 1 else fibrec(n-2) + fibrec(n-1)
fun main(x:int):int = let n = ref 1 in while (!n < 11) do (printint(fibrec(!n)) ; n := !n + 1) ; 0

