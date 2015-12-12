fun main(argc:int):int =
let a = ref 0 in
let b = ref 1 in
let n = ref 1 in
while !n < 10 do (let tmp = !b in (b:=!a+!b; a:=tmp; n:=!n+1)); printint(!b);0
