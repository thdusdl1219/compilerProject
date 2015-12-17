
1. Retarget to pentium
(* 대부분의 파일 변경, x86.sml, x86.sig 추가 *)

기존의 target이었던 mips instruction을 x86 instruction으로 바꾸었습니다.
as9의 결과 파일들을 최대한 이용하기 위해서 이 컴파일러의 IR이라고 볼 수 있는 Mips.instruction의 구조를
최대한 바꾸지 않으면서 (def, use를 최대한 지키려고 노력함) x86 instruction으로 구현하였습니다.
여기서 힘들었던 점이 몇가지 있었는데

1. mips는 RISC, x86은 CISC
mips는 register의 수가 많고 x86은 register의 수는 적지만 지원하는 instruction이 많다.
이로 인해 아래의 많은 문제들이 파생되었다..

2. mips는 함수의 인자를 register로 전달하지만 x86으로 구현하면 stack으로 전달하게 바꾸어야 함. 
함수 call과 관련된 codegen.sml를 calling convention을 지키도록 변경하였다.

3. 기본적으로 지원하는 함수들의 구현(alloc, printint)
처음에는 sys_write와 sys_brk system call을 이용하여 구현하려고 하였다. 
하지만 sys_write는 int 출력을 위한 함수가 아니여서 많은 어려움이 있었고
두 system call 모두 많은 register를 사용하여서 많은 register의 저장과 복구가 요구되었다.
그래서 library 함수인 malloc과 printf를 이용하여 alloc과 printint를 구현하였다.

4. mips와 x86의 instruction간의 1대1 매칭이 안됨
mips에서 지원하지만 x86에서 지원하지 않는 instruction들이 존재하였다.
이를 해결하기 위해 x86 instruction을 이용하여 구현하였다..
(x86.sml 참고..)

4. 기존의 파일들을 이용하기 위해서..
mips instruction들을 x86 instruction을 이용하여 구현할때 use, def를 지키면서 
sourse register들을 오염하지 않도록 구현해야했다. 
assembly로 코딩하던 시절의 프로그래머들의 힘듬을 알 것 같다. 컴파일러 만든 사람 짱짱맨


그래서 이 컴파일러를 돌리고 나온 .s파일을 gcc로 컴파일 후 (-m32 옵션을 줘야함) x86 machine에서 돌릴 수 있습니다.
이를 쉽게 하기 위해서 so.py를 만들었습니다.

./so.py (-c) (testfile명 or testfile들이 들어있는 directory) -a
-c 옵션을 주지 않으면 우리의 컴파일러 + gcc로 컴파일 된 executable file을 생성합니다.
-c 옵션을 주면 executable file 생성 및 기존의 mips.py로 생성하였던 output들과 비교할 수 있습니다.

2. deadcode elimination
(* deadcode.sml 추가 *)

live하지 않는 def를 하면서 side effect가 없는 instruction들을 제거합니다. 
