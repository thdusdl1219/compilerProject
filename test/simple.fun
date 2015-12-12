fun simple(x:int):int = x+1
fun tuple(x:int):<int,int,<int,int>,<>> = <x,x,<x,x>,<>>
fun proj(x:<int,int>):int = #0 x
fun iff(x:int):int = if #0 <1,0,3> then x else 1
fun whilef(x:int ref):<> = while !x < 10 do (x := !x + 1; printint(!x
))
fun letf(x:int):int = let p = x + 3 in let v = p+3 in v+2
fun call(x:int):int = x
fun reff(x:int):int = let r = ref 2 in !r + 3

fun main(x:int):int =
	printint(simple(3));
	printint(#1 #2 tuple(4));
	printint(proj(<1,2>));
	printint(iff(16));
	whilef(ref 1);
	printint(letf(3));
	printint(call(3));
	reff(4)