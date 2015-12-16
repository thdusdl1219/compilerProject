fun int(a:<int>):int = 
	#0 a

fun foo(x:int):int ref = ref (x:int)
	
fun fact(n:int) : int =
	if n=0 then 1 else n * fact(n-1)

fun addsome (x:int):int = x +5
fun add (x:<int,int>):int = #0 x + #1 x
fun gt (t:<int,int>):int = 
    let a = #0 t in
    let b = #1 t in 
      not (a < b & not a = b)

fun test(arg:<int, int, int ref>):<>=
	let result = #0 arg in
	let expected = #1 arg in
	let sum = #2 arg in
	let cur = !sum in
	if not result=expected then 
		(printint(result);sum := cur + 1;<>)
		else
		(printint(result);<>)

fun main(arg:int):int =
	let failed = ref 0 in
	let True=1 in
	let False=0 in

	/* factorial */
	test(<let res = fact(5)
	in (printint(res); res), 120, failed>);
	
	/* if-then-else */
	test(<if True & True then (if True || False then False || True else False) else False || False, True, failed>);
	
	/* not & or */
	test(<not False || True,True,failed>);
	
	/* multiple NOTs */
	test(<not not not True, False, failed>);
	
	/* bin, unary op priority */
	test(<-1 * 2 - 3,-5,failed>);
	test(<-1*-5+2-7*3,-14,failed>);

	/* while-do */
	test(<(
		let sum = ref 0 in
		let a = ref 10 in
		while !a do
			(let v = !a in
			a := v-1;
			let sumv = !sum in
				sum := sumv + v)
		; !sum), 55, failed
	>);
	
	/* sequence of expressions */
	test(<let tmp = 1 in tmp - 2 ; tmp+ 2,3,failed>);
	
	/* Function nammed "int" */
	test(<let a = int(<1>) + int(<2>) in a + 6 * 2, 15,failed>);
	
	/* function call is the first! */
	test(<let x = 16 in !foo(x),16,failed>);
	
	/* Associativity */
	test(<
		let a = ref 0 in
		a := 1;
		a := 2;
		a := 3;
		a := 4;
		!a, 4, failed
	>);
	test(<1 + - - - 1, 0,failed>);

	/* */
	test(<
		let x = (5 + 3) * 7 in
		let y = x + 7 in
		  if gt (<x, 0>) then 
		    if not x = 7 then
		      addsome(add(<x,y>))
		    else -1
		  else if -2 then 0 else 1

		/*
		fun decfac(x:int):int = fac (x-1)
		/*
		fun fac (x) = 
		  if x = 0 then 1
		  else x * decfac(x)
		*/
		in 
		 print 
		 let y = decfac(1) in
		 fac (6)  	
		*/,124,failed
	>);
	!failed
