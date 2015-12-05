1.  Implement additional maximum munch algorithm in codegen.sml

========================================

1-1) sub(r, const1, const2) => Li(r, const1 - const2)
1-2) sub(r1, r2, const) => Addi(r1, r2, -const2)

| A.Sub =>
+  let val mop = fun2mips_arith_op(oper); val r = M.newReg()
+  in if(List.nth(expl, 0) = A.Int 0) 
+  then
+    (case List.nth(expl, 1) of
+          A.Int i => (emit(M.Li(r, M.immed (i * (~1)))); r) 
+        | _ => (emit(M.Arith2(M.Neg, r, gen_exp env (List.nth(expl, 1)) )); r) 
+   )
+  else
+    (case (List.nth(expl, 0), List.nth(expl, 1)) of
+          (A.Int i, A.Int i2) => (emit(M.Li(r,M.immed (i - i2)));r)
+        | (_, A.Int i) => (emit(M.Arithi(M.Addi, r, gen_exp env (List.nth(expl, 0)), M.immed (
+        | (_, _) =>  (emit(M.Arith3(mop,r,gen_exp env (List.nth(expl, 0)),gen_exp env (List.nt
+    )
+  end

2-1) If(exp, 1, 0) => Move(result, exp_result) 
2-2) If(exp, 0, 1) => Seq(result, exp_result, $zero)

| gen (A.If (exp1, exp2, exp3)) =
    let val else_lab = M.freshlab (); val r = M.newReg(); val done_lab = M.freshlab ()
-   in emit(M.Branchz(M.Eq, gen_exp env exp1, else_lab)); emit(M.Move(r, gen_exp env exp2)) ; emit(M.J(done
-   emit_label (else_lab) ; emit(M.Move(r, gen_exp env exp3)) ; emit_label (done_lab); r end
+ in (
+   case (exp2, exp3) of
+        (A.Int(0), A.Int(1)) => 
+         (emit(M.Arith3(M.Seq, r, gen_exp env exp1, M.reg "$zero")); r)
+      | (A.Int(1), A.Int(0)) => (emit(M.Move(r, gen_exp env exp1)); r)
+      | _ =>
+         (emit(M.Branchz(M.Eq, gen_exp env exp1, else_lab)); emit(M.Move(r, gen_exp env exp2)) ; emit(M.J(
+   ) 
+ end

========================


2. Implement coalescing to eliminate redundant move instructions in color.sml.

3. Implement 2nd general spilling method in general spilling in regalloc.sml.


