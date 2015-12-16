fun apple (a:<int, int>) : <int, int> =
  if((#0 a) = 1)
  then  <#1 a,#0 a>
  else
    a

fun banana (b:<int, int>) : int =
  (#1 b) * 4 + (#1 b)

fun main(argc: int): int =
  let a = ref 1 in
  let b = ref 10 in
    printint(banana(apple(<!a, !b>))); !a
