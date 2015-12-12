/* program that includes all the syntactic constructs
 * correct result: print out 700; returns <> and L0 = 0; L1 = 700 */

fun int_ops(x:int):int =
  let a = x + 5 * 3 in
  let b = a - -1 in
  let c = b - (16) in
    if ((c < 10 || not (c < 10)) & x=c ) then x
    else 0

fun dec_ref(x:int ref):<> = 
  let a = !x in
    (x := a - 1; <>)

fun fact(x:int ref):int = 
  if (!x) < 1 then 1
    else let a = !x in
         let b = dec_ref(x) in
           a * fact(x) 

fun add_pair(x:<int,int>):(int) = 
   let a = #0 x in
   let b = #1 x in
     a + b

fun add_pair_silly(x:<>):<int,int>->int = 
   add_pair

fun loop(x:int):int = 
   let a = ref (x:int) in
     ((while not((!a) < 701) do a:=(!a)-1); !a)


fun main(arg:int) : int =
  let z = int_ops(add_pair_silly (<>) (<2,4>)) in
  let a = fact(ref(z:int)) in
  let b = loop(a) in
     (printint(b); b)

