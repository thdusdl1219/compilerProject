/*return: ceiling of square root of num*/
fun sqrt_ceil_iter(num:int):int =
let i=ref 0 in
(while (!i*!i)<num do
	i:=!i+1);
	!i

fun distance(xy12:<<int,int>,<int,int>>):int=
let xy1=#0 xy12 in
let xy2=#1 xy12 in
sqrt_ceil_iter(((#0 xy1)-(#0 xy2))*((#0 xy1)-(#0 xy2))+((#1 xy1)-(#1 xy2))*((#1 xy1)-(#1 xy2)))


fun main(argc:int):int=
printint(distance (<<7,5>,<3,8>>));0
