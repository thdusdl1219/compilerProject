fun main(argc:int):int =
let a = ref 1 in
let b = ref(if !a = 1 then 1 else (a:=0;0)) in
let c = ref(if !b = 0 then (b:=0;0) else 1) in
let d = ref(!c || (c:=0;0)) in
let e = ref 0 in
let f = ref(!d & (e:=1;1)) in
printint(!a + !b + !c + !d + !e + !f);0

