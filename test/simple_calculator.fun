fun add (x:<int, int>):int = (#0 x) + (#1 x)
fun sub (x:<int, int>):int = (#0 x) - (#1 x)
fun mul (x:<int, int>):int = (#0 x) * (#1 x)

fun func (tp: int): <int, int>->int = 
  if tp = 1 then add
  else 
    if tp = 2 then sub
    else mul
      
fun main(argc: int):int =
let a = ref 1 in 
let b = ref 2 in
let c = ref 3 in
let x = 20 in
let y = 5 in
printint (func(!a)(<x,y>)); printint (func(!b)(<x,y>)); printint (func(!c)(<x,y>)); 0
