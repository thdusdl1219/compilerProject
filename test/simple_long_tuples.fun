fun long_tuples(x:<int,int,int,int,int>):int=
(#0 x) * (#1 x) * (#2 x) * (#3 x) * (#4 x)

fun main(argc:int):int =
printint(long_tuples(<1,2,3,4,5>));0
