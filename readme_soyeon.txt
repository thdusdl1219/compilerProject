
1. Retarget to pentium
(* ��κ��� ���� ����, x86.sml, x86.sig �߰� *)

������ target�̾��� mips instruction�� x86 instruction���� �ٲپ����ϴ�.
as9�� ��� ���ϵ��� �ִ��� �̿��ϱ� ���ؼ� �� �����Ϸ��� IR�̶�� �� �� �ִ� Mips.instruction�� ������
�ִ��� �ٲ��� �����鼭 (def, use�� �ִ��� ��Ű���� �����) x86 instruction���� �����Ͽ����ϴ�.
���⼭ ������� ���� ��� �־��µ�

1. mips�� RISC, x86�� CISC
mips�� register�� ���� ���� x86�� register�� ���� ������ �����ϴ� instruction�� ����.
�̷� ���� �Ʒ��� ���� �������� �Ļ��Ǿ���..

2. mips�� �Լ��� ���ڸ� register�� ���������� x86���� �����ϸ� stack���� �����ϰ� �ٲپ�� ��. 
�Լ� call�� ���õ� codegen.sml�� calling convention�� ��Ű���� �����Ͽ���.

3. �⺻������ �����ϴ� �Լ����� ����(alloc, printint)
ó������ sys_write�� sys_brk system call�� �̿��Ͽ� �����Ϸ��� �Ͽ���. 
������ sys_write�� int ����� ���� �Լ��� �ƴϿ��� ���� ������� �־���
�� system call ��� ���� register�� ����Ͽ��� ���� register�� ����� ������ �䱸�Ǿ���.
�׷��� library �Լ��� malloc�� printf�� �̿��Ͽ� alloc�� printint�� �����Ͽ���.

4. mips�� x86�� instruction���� 1��1 ��Ī�� �ȵ�
mips���� ���������� x86���� �������� �ʴ� instruction���� �����Ͽ���.
�̸� �ذ��ϱ� ���� x86 instruction�� �̿��Ͽ� �����Ͽ���..
(x86.sml ����..)

4. ������ ���ϵ��� �̿��ϱ� ���ؼ�..
mips instruction���� x86 instruction�� �̿��Ͽ� �����Ҷ� use, def�� ��Ű�鼭 
sourse register���� �������� �ʵ��� �����ؾ��ߴ�. 
assembly�� �ڵ��ϴ� ������ ���α׷��ӵ��� ������ �� �� ����. �����Ϸ� ���� ��� ¯¯��


�׷��� �� �����Ϸ��� ������ ���� .s������ gcc�� ������ �� (-m32 �ɼ��� �����) x86 machine���� ���� �� �ֽ��ϴ�.
�̸� ���� �ϱ� ���ؼ� so.py�� ��������ϴ�.

./so.py (-c) (testfile�� or testfile���� ����ִ� directory) -a
-c �ɼ��� ���� ������ �츮�� �����Ϸ� + gcc�� ������ �� executable file�� �����մϴ�.
-c �ɼ��� �ָ� executable file ���� �� ������ mips.py�� �����Ͽ��� output��� ���� �� �ֽ��ϴ�.

2. deadcode elimination
(* deadcode.sml �߰� *)

live���� �ʴ� def�� �ϸ鼭 side effect�� ���� instruction���� �����մϴ�. 
