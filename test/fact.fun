fun factorial(n : int):int = 
  if (n = 0) then 1
  else if (n < 0) then 1
  else n * factorial(n - 1)

fun main(argc:int):int = 
  printint(factorial(10)); 0
