COMPILER PROJECT 

author: Soyeon Park, Dayeol Lee
기본 골격은 Soyeon Park의 코드를 사용하였음!

1. Summary
 
 구현 내용은 크게 아래와 같다.
 - Retarget To Pentium
 - Loop Optimization
 - Dead Code Elimination
 - 약간의 Maximal Munch 추가

2. Retarget to pentium
	(* 대부분의 파일 변경, x86.sml, x86.sig 추가 *)

	기존의 target이었던 mips instruction을 x86 instruction으로 바꾸었습니다.
	as9의 결과 파일들을 최대한 이용하기 위해서 이 컴파일러의 IR이라고 볼 수 있는 Mips.instruction의 구조를
	최대한 바꾸지 않으면서 (def, use를 최대한 지키려고 노력함) x86 instruction으로 구현하였습니다.
	여기서 힘들었던 점이 몇가지 있었는데

	(1) mips는 RISC, x86은 CISC
	mips는 register의 수가 많고 x86은 register의 수는 적지만 지원하는 instruction이 많다.
	이로 인해 아래의 많은 문제들이 파생되었다..

	(2) mips는 함수의 인자를 register로 전달하지만 x86으로 구현하면 stack으로 전달하게 바꾸어야 함. 
	함수 call과 관련된 codegen.sml를 calling convention을 지키도록 변경하였다.

	(3) 기본적으로 지원하는 함수들의 구현(alloc, printint)
	처음에는 sys_write와 sys_brk system call을 이용하여 구현하려고 하였다. 
	하지만 sys_write는 int 출력을 위한 함수가 아니여서 많은 어려움이 있었고
  sys_brk를 사용하여 구현하려면 sys_mprotect도 실행해 줘야해서 매우 복잡한 assembly 코딩이 요구되었다..ㅠㅠ
  (brk로 heap 할당만 하고 mprotect로 write|read 권한을 주지 않으면 segmentation fault가 난다..)
	또한 두 system call 모두 많은 register를 사용하여서 많은 register의 저장과 복구가 요구되었다.
	그래서 library 함수인 malloc과 printf를 이용하여 alloc과 printint를 구현하였다.

	(4) mips와 x86의 instruction간의 1대1 매칭이 안됨
	mips에서 지원하지만 x86에서 지원하지 않는 instruction들이 존재하였다.
	이를 해결하기 위해 x86 instruction을 이용하여 구현하였다..
	(x86.sml 참고..)

	(5) 기존의 파일들을 이용하기 위해서..
	mips instruction들을 x86 instruction을 이용하여 구현할때 use, def를 지키면서 
	sourse register들을 오염하지 않도록 구현해야했다. 
	assembly로 코딩하던 시절의 프로그래머들의 힘듬을 알 것 같다. 컴파일러 만든 사람 짱짱맨

	그래서 이 컴파일러를 돌리고 나온 .s파일을 gcc로 컴파일 후 (-m32 옵션을 줘야함) x86 machine에서 돌릴 수 있습니다.
	이를 쉽게 하기 위해서 so.py를 만들었습니다.

	./so.py (-c) (testfile명 or testfile들이 들어있는 directory) -a
	-c 옵션을 주지 않으면 우리의 컴파일러 + gcc로 컴파일 된 executable file을 생성합니다.
	-c 옵션을 주면 executable file 생성 및 기존의 mips.py로 생성하였던 output들과 비교할 수 있습니다.

	****** 결과
	우리의 Compiler를 이용해서 gcc로 컴파일 할 수 있는 x86 assembly 코드를 만들 수 있다.

3. Loop Optimization
	(* loopopt.sml *)
	Loop Optimization을 구현하였는데, fun 특성상 loop induction variable에 대한 optimization은 하지 않았다.
	fun에서는 variable이 variable이 아니기 때문에 모든 variable은 메모리에 저장됨.
	만약 induction variable analysis를 하고 싶으면 alias analysis 역시 수행해야 함.
	따라서 거기까지는 하지 못하였고,  Loop Invariant Code Motion (LICM)만 구현함.
	수 많은 삽질 끝에 LICM을 완성하였다!

	****** 구현 내용 정리
	(1) Basic Block 만들기
	Codegen에서 우리는 모든 Block이 Basic Block (single entry, single exit)이 되도록 만들기 위해서 Branch나 Jump가 있을 때 마다 Label로 다음 block과 구분을 시켜주었다. 이러한 간단한 수정으로 Basic Block을 기본 단위로 한 CFG를 구현할 수 있었다.
	(2) CFG
	CFG를 만들어 주었다. fun code에서 모든 block을 돌면서 CFG에 edge를 추가해 주었다. CFG는 IG와 마찬가지로 그래프 인터페이스를 썼고, 노드가 register가 아닌 codeblock인 그래프로 만들었다. CFG 만드는 함수는 LoopOpt.makeCFG 이다.
	(3) Domination Tree
	Domtree를 만드는 것은 책에서 소개된 알고리즘을 그대로 썼다. 사실 DomTree 까지는 필요 없고 Domination 관계만 알면 된다. 그래서 DomMap을 만들었다. 함수는 LoopOpt.makeDomMap 이다.
	(4) Finding Loops
	Domination 관계를 알았으니 이제 Back Edge를 찾아서 Loop 를 찾아야 한다. Loop를 찾는것은 우선 CFG에 있는 edge들을 보고 Dominator로 가는 edge가 있는 경우를 찾았다. 이렇게 하면 간단하게 header는 찾을 수 있는데, 문제는 Loop Body를 찾는 것이었다.
	Loop Body는 여러 고민 끝에 아래와 같은 방법을 사용했다.

	for back edge <n,h>, do reverse DFS on CFG from n to h

	즉, n에서 CFG의 모든 앳지가 반대로 그려진 reversed CFG에서 Depth-first search (DFS)를 해서 h를 만날 때 까지 모든 노드를 마크한다. DFS에서 거쳐간 모든 노드들이 곧 Loop Body가 된다.
	이렇게 Loop Header와 Loop Body를 모두 찾아내어 리스트에 저장해주었다.

	(5) Find Invariants
	Loop를 찾았으니 Invariant를 찾아야 한다. Loop Invariant의 정의는 아래와 같다.
	* t: d = oper(s1,s2, ...) 에 대해서
	 - all of the uses are
	 	1) constant or
	 	2) invariant or
	 	3) defined outside the loop
	따라서 Loop 내의 모든 instruction에 대해서 위와 같은 조건을 만족하는 invariant register set을 구했다.
	즉, use가 constant이거나, invariant이거나, loop 밖에서 정의되어 있으면 d가 invariant register 인 것이다.
	이것은 여러 회에 걸쳐서 해주어야 한다. 이유는, 아래와 같은 코드의 경우이다.
	  t1: d1 = 1
	  t2: d2 = d1 + 3
	  t3: d3 = d1 + d2
	위의 경우, t1은 invariant인데, t2, t3가 invariant 코드인지 알기 위해서는 t1이 invariant라는 것을 확인하고 나서 가능하다.
	따라서 이를 recursive하게 구현해주었다.

	(6) Check for LICM constraints
	이제 해당 Invariant가 LICM이 가능한지를 확인해야 한다. 아래와 같은 3가지 constraint가 있다.
	  1) No other defs in the loop
	  2) invariant must not be live-out of loop exits
	  3) invariant must not be live-out of loop preheader
	fun 코드의 특성 상 2)번은 확인하지 않아도 무방하다. 2)의 경우, loop 중간에 exit 포인트가 많이 있는 경우에 확인해야 하는데, fun의 경우에는 break문이 없기 때문에 loop exit을 우리가 알고 있고, 한 블럭이다. 따라서 1), 3)만 확인해주었다.
	  1) 의 경우에는, 모든 Loop 내의 instruction에서 여러번 정의된 Register를 (5)에서 구한 invariant에서 빼주었다.
	  2) 의 경우, Liveness.analyze 함수를 약간 수정하여 header block의 live-in을 확인할 수 있게 했다. header block의 live-in을 2)에서 구한 invariant에서 빼주었다.
	Constraint를 확인하고 나면 LICM이 가능한 register가 무엇인지 정확하게 알 수 있다. 따라서 그 다음에는 Fun Code를 수정한다.

	(7) Do LICM!!
	Loop와 loop별 LICM 대상을 알고 있기 때문에 그리 어렵지는 않았다. 단지 loop마다 모든 block을 읽어와서 funCode에서 해당되는 block에서 LICM 코드를 뽑아내어 preheader에 들어갈 instruction으로 모았다.
	그 다음에는 Preheader를 Loop Header 앞에 붙이고, 기존 Block에서 LICM 대상 instruction들을 삭제해 주었다.

	***** 결과
	아래 코드를 참조하라. 
	To 조교님: 직접 코드를 비교하고 싶으시면 작업 directory에서 sml을 실행하고 아래와 같은 명령을 실행하세요.
	 - CM.make "sources.cm";
	 - Compile.compile "test/loop.fun";
	
	loop.fun.noloopopt.1.s 파일과
	loop.fun.noregalloc.s 파일, 그리고
	loop.fun.s 파일이 생성되었을 것입니다.
	loop.fun.s 는 최종 결과이고, loop.fun.noloopopt.1.s는 Loop Optimize를 하기 전의 코드, loop.fun.noregalloc.s는 Loop Optimize를 한 이후의 코드입니다.

	(1) LICM 전 (while 문 내부)

			L2:
				movl 	0($x12), $x18	# $x18 := [$x12+0]
				movl 	$100, $x19	# $x19 := $100
				cmp 	$x19, $x18
				jge 	L3
			L4:
				movl 	0($x12), $x20	# $x20 := [$x12+0]
				movl	$x20, $x21	#$x21 := $x20
				add 	$1, $x21	# $x20 := $x20+$1
				movl 	$x21, 0($x12)	# [$x12+0] := $x21
				movl 	0($x9), $x22	# $x22 := [$x9+0]
				movl 	$3, $x23	# $x23 := $3
				movl	$x16, $x24	#$x24 := $x16
				add	$x15, $x24	# $x24 := $x15+$x16
				movl 	0($x12), $x26	# $x26 := [$x12+0]
				cmp 	$x23, $x26
				jge 	L5
			L7:
				movl	$x20, $x28	#$x28 := $x20
				add	$x22, $x28	# $x28 := $x22+$x20
				movl	$x15, $x27	#$x27 := $x15
				add	$x28, $x27	# $x27 := $x28+$x15
				movl 	$x27, 0($x9)	# [$x9+0] := $x27
				movl	$x27, $x25	#$x25 := $x27
				jmp 	L6		# goto L6
			L5:
				movl	$x24, $x29	#$x29 := $x24
				add	$x22, $x29	# $x29 := $x22+$x24
				movl 	$x29, 0($x9)	# [$x9+0] := $x29
				movl	$x29, $x25	#$x25 := $x29
			L6:
				movl	$x25, $x17	#$x17 := $x25
				jmp 	L2		# goto L2

	(2) LICM 후 (while 문 내부 + preheader)

			L8:
				movl	$x16, $x24	#$x24 := $x16
				add	$x15, $x24	# $x24 := $x15+$x16
				movl 	$3, $x23	# $x23 := $3
				movl 	$100, $x19	# $x19 := $100
			L2:
				movl 	0($x12), $x18	# $x18 := [$x12+0]
				cmp 	$x19, $x18
				jge 	L3
			L4:
				movl 	0($x12), $x20	# $x20 := [$x12+0]
				movl	$x20, $x21	#$x21 := $x20
				add 	$1, $x21	# $x20 := $x20+$1
				movl 	$x21, 0($x12)	# [$x12+0] := $x21
				movl 	0($x9), $x22	# $x22 := [$x9+0]
				movl 	0($x12), $x26	# $x26 := [$x12+0]
				cmp 	$x23, $x26
				jge 	L5
			L7:
				movl	$x20, $x28	#$x28 := $x20
				add	$x22, $x28	# $x28 := $x22+$x20
				movl	$x15, $x27	#$x27 := $x15
				add	$x28, $x27	# $x27 := $x28+$x15
				movl 	$x27, 0($x9)	# [$x9+0] := $x27
				movl	$x27, $x25	#$x25 := $x27
				jmp 	L6		# goto L6
			L5:
				movl	$x24, $x29	#$x29 := $x24
				add	$x22, $x29	# $x29 := $x22+$x24
				movl 	$x29, 0($x9)	# [$x9+0] := $x29
				movl	$x29, $x25	#$x25 := $x29
			L6:
				movl	$x25, $x17	#$x17 := $x25
				jmp 	L2		# goto L2

 	추가된 L8 블록은 Loop Preheader이며, $x19, $x23, $x24가 여기로 이동되었음을 알 수 있습니다.

3. Deadcode Elimination
	(* deadcode.sml 추가 *)
	live하지 않는 def를 하면서 side effect가 없는 instruction들을 제거합니다.

4. Maximal Munch
	몇가지 Maximal Munch를 추가했습니다.
	- If( Op(LT, ... ), ... )
	- If( Op(Eq, ...), ...)
	- While( Op(LT, ...), ...)
	- While( Op(LT, ...), ...)
	
