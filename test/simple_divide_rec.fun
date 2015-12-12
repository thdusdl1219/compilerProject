/*return:(Quotient,remainder)*/
fun divide_rec(num_num:<int,int>):<int,int> =
let dividend=#0 num_num in
let divisor=#1 num_num in
	if dividend<divisor then <0,dividend>
	else 
	 let ret=divide_rec(<dividend-divisor,divisor>) in
			<#0 ret+1,#1 ret>

fun main(argc:int):int =
	printint(#0 divide_rec(<1000,17>));
	printint(#1 divide_rec(<1000,17>));0
