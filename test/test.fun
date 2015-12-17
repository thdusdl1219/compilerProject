fun addsome (x:int):int = x +5
fun add (x:<int,int>):int = #0 x + #1 x
fun gt (t:<int,int>):int = 
    let a = #0 t in
    let b = #1 t in 
      not (a < b & not a = b)
fun main (argc : int) : int =
let x = (5 + 3) * 7 in
let y = x + 7 in
  if gt (<x, 0>) then 
    if not x = 7 then
      (printint(addsome(add(<x,y>)));0)
    else (printint(-1);-1)
  else if -2 then (printint(0);0) else (printint(1);1)

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
*/


