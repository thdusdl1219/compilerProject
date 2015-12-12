fun addsome(x:int):int = x+5

fun add(x:<int,int>):int = #0 x + #1 x

fun gt(t:<int,int>):int =
	let a = #0 t in
	let b = #1 t in
	not (a<b & not a=b)

fun main(argc:int):int =
	let x = (5 + 3) * 7 in
	let y = x + 7 in
printint(
				if gt(<x,0>) then
		if not x = 7 then
			addsome(add(<x,y>))
		else
			-1
	else
		if -2 then
			0
		else
			1
		);0
