fun get(x: <<int ref, int ref, int ref, int ref, int ref>, int>): int ref = 
  let target = #0 x in
  let i = #1 x in
  if (i = 0) then #0 target
  else
    if (i = 1) then #1 target
    else 
      if (i = 2) then #2 target
      else
        if (i = 3) then #3 target
        else #4 target

fun swap(x: <int ref, int ref>):<> =
  let a = #0 x in
  let b = #1 x in
  let temp = !a in
  (a:=!b; b:=temp; <>)

fun sort(x: <int ref, int ref, int ref, int ref, int ref>): <> =
  let i = ref 0 in
  let j = ref 0 in
  
  while (!i < 4)
  do (
    j := 0;
    (while (!j < 4-!i)
     do
     (if (not (!get(<x,!j>)<!get(<x,!j+1>))) then swap(<get(<x, !j>), get(<x, !j+1>)>);
     j := !j+1));
    i := !i+1
  )

fun main(x: int): int = 
  let x = <ref 5, ref 2, ref 3, ref 4, ref 1> in
  sort(x); 
  printint(!(#0 x)); 
  printint(!(#1 x)); 
  printint(!(#2 x)); 
  printint(!(#3 x)); 
  printint(!(#4 x));
  0


