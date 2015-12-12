/*return: (Quotient,remainder)*/
fun divide_iter(num_num:<int,int>):<int,int> =
let dividend=ref (#0 num_num) in
let divisor=#1 num_num in
let Quotient=ref 0 in
	(while not ((!dividend)<divisor) do
	 (dividend:=!dividend-divisor;
	  Quotient:=!Quotient+1)
	;<!Quotient,!dividend>)
		
fun main(argc:int):int =
	let ret= divide_iter(<10000,17>) in
		printint(#0 ret);
		printint(#1 ret);0
