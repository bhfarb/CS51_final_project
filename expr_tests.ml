
open Test_simple ;;      (* a really simple unit testing framework *)
open Expr ;;

let test () =
  (* exp_to_abstract_string test*)
  unit_test (exp_to_abstract_string (Num (1)) = "Num(1)") "e_to_ab_s num";
  unit_test (exp_to_abstract_string (Bool (true)) = "Bool(true)") "e_to_ab_s bool true";
  unit_test (exp_to_abstract_string (Var ("x")) = "Var(x)") "e_to_ab_s var";
  unit_test (exp_to_abstract_string (Unop(Negate, Num(1))) = "Unop(Negate, Num(1))") "e_to_ab_s negate";
  unit_test (exp_to_abstract_string (Binop(Plus, Num(1), Var("x"))) = "Binop(Plus, Num(1), Var(x))") "e_to_ab_s plus";
  unit_test (exp_to_abstract_string (Binop(Equals, Num(1), Var("x"))) = "Binop(Equals, Num(1), Var(x))") "e_to_ab_s equals";
  unit_test (exp_to_abstract_string (Binop(LessThan, Num(1), Var("x"))) = "Binop(LessThan, Num(1), Var(x))") "e_to_ab_s less than";
  unit_test (exp_to_abstract_string (Binop(Minus, Num(3), Var("x"))) = "Binop(Minus, Num(3), Var(x))") "e_to_ab_s minus";
  unit_test (exp_to_abstract_string (App(Fun("x", Var("x")), Num(5))) = "App(Fun(x, Var(x)), Num(5))") "e_to_ab_s app1";
  unit_test (exp_to_abstract_string (App(Fun("x", Binop(Plus, Var("x"), Var("z"))), Var("y"))) = "App(Fun(x, Binop(Plus, Var(x), Var(z))), Var(y))") "e_to_ab_s app2";
  unit_test (exp_to_abstract_string (Fun("x", Num(5))) = "Fun(x, Num(5))") "e_to_ab_s fun";
  unit_test (exp_to_abstract_string (Let("a", Bool(true), Num(8))) = "Let(a, Bool(true), Num(8))") "e_to_ab_s let";
  unit_test (exp_to_abstract_string (Letrec("x", Fun("x", Var("x")), Num(8))) = "Letrec(x, Fun(x, Var(x)), Num(8))") "e_to_ab_s let rec";
  unit_test (exp_to_abstract_string (Conditional(Bool(true), Var("x"), Var("y"))) = "Conditional(Bool(true), Var(x), Var(y))") "e_to_ab_s conditional";

  (* free_vars tests *)
  unit_test (same_vars (free_vars (Num (1))) (vars_of_list [])) "free_vars num";
  unit_test (same_vars (free_vars (Bool (true))) (vars_of_list [])) "free_vars bool true";
  unit_test (same_vars (free_vars (Bool (false))) (vars_of_list [])) "free_vars bool false";
  unit_test (same_vars (free_vars (Var ("x"))) (vars_of_list ["x"])) "free_vars var";
  unit_test (same_vars (free_vars (Binop(Plus, Num(1), Var("x")))) (vars_of_list ["x"])) "free_vars binop 1 + x";
  unit_test (same_vars (free_vars (Unop(Negate, Num(1)))) (vars_of_list [])) "free_vars unop none";
  unit_test (same_vars (free_vars (Unop(Negate, Var("y")))) (vars_of_list ["y"])) "free_vars unop y";
  unit_test (same_vars (free_vars (Binop(Plus, Var("y"), Var("x")))) (vars_of_list ["x";"y"])) "free_vars binop x + y";
  unit_test (same_vars (free_vars (Binop(Plus, Var("y"), Var("x")))) (vars_of_list ["x";"y"])) "free_vars binop x + y";
  unit_test (same_vars (free_vars (Binop(Equals, Var("y"), Var("x")))) (vars_of_list ["x";"y"])) "free_vars binop x = y";
  unit_test (same_vars (free_vars (Binop(Equals, Num(1), Num(3)))) (vars_of_list [])) "free_vars binop 1 = 3";
  unit_test (same_vars (free_vars (App(Fun("x", Var("x")), Num(5)))) (vars_of_list [])) "free_vars app (fun x -> x) 5";
  unit_test (same_vars (free_vars (App(Fun("x", Var("x")), Var("x")))) (vars_of_list ["x"])) "free_vars app (fun x -> x) x";
  unit_test (same_vars (free_vars (App(Fun("x", Var("x")), Var("y")))) (vars_of_list ["y"])) "free_vars app (fun x -> x) y";
  unit_test (same_vars (free_vars (App(Fun("x", Binop(Plus, Var("x"), Num(1))), Var("y")))) (vars_of_list ["y"])) "free_vars app (fun x -> x + 1) y";
  unit_test (same_vars (free_vars (App(Fun("x", Binop(Plus, Var("x"), Var("z"))), Var("y")))) (vars_of_list ["y"; "z"])) "free_vars app (fun x -> x + z) y";
  unit_test (same_vars (free_vars (App(Fun("x", Num(5)), Var("y")))) (vars_of_list ["y"])) "free_vars app (fun x -> 5) y";
  unit_test (same_vars (free_vars (Fun("x", Num(5)))) (vars_of_list [])) "free_vars fun x -> 5";
  unit_test (same_vars (free_vars (Fun("x", Var("x")))) (vars_of_list [])) "free_vars fun x -> x";
  unit_test (same_vars (free_vars (Fun("x", Var("y")))) (vars_of_list ["y"])) "free_vars fun x -> y";
  unit_test (same_vars (free_vars (Fun("y", Binop(Plus, Var("y"), Var("x"))))) (vars_of_list ["x"])) "free_vars fun y -> y + x";
  unit_test (same_vars (free_vars (Fun("y", Binop(Plus, Var("y"), Num(5))))) (vars_of_list [])) "free_vars fun y -> y + 5";
  unit_test (same_vars (free_vars (Let("y", Binop(Plus, Var("x"), Num(5)), Binop(Plus, Var("y"), Var("x"))))) (vars_of_list ["x"])) "free_vars let y = x + 5 in x + y";
  unit_test (same_vars (free_vars (Let("y", Binop(Plus, Var("x"), Num(5)), Binop(Plus, Var("y"), Num(5))))) (vars_of_list ["x"])) "free_vars let y = x + 5 in y + 5";
  unit_test (same_vars (free_vars (Let("y", Num(5), Binop(Plus, Var("y"), Num(5))))) (vars_of_list [])) "free_vars let y = 5 in y + 5";
  unit_test (same_vars (free_vars (Let("y", Num(5), Binop(Plus, Var("y"), Var("x"))))) (vars_of_list ["x"])) "free_vars let y = 5 in y + x";

  (* new_varname tests *)
  unit_test (new_varname () = "v0") "new_varname 1";
  unit_test (new_varname () = "v1") "new_varname 2";

  (* subst tests *)
  unit_test ((subst "x" (Num(5)) (Num(1))) = Num(1)) "subst x 1 1";
  unit_test ((subst "y" (Num(1)) (Num(1))) = Num(1)) "subst y 5 1";
  unit_test ((subst "x" (Num(5)) (Var("x"))) = Num(5)) "subst x 5 x";
  unit_test ((subst "x" (Num(5)) (Var("y"))) = Var("y")) "subst x 5 y";
  unit_test ((subst "x" (Num(5)) (Var("y"))) = Var("y")) "subst x 5 y";
  unit_test ((subst "y" (Var("x")) (Var("y"))) = Var("x")) "subst y x y";
  unit_test ((subst "y" (Binop(Plus, Var("a"), Var("b"))) (Var("y"))) = (Binop(Plus, Var("a"), Var("b")))) "subst y (a + b) y";
  unit_test ((subst "y"  (Var("x")) (Binop(Plus, Var("a"), Var("b")))) = Binop(Plus, Var("a"), Var("b"))) "subst y x (a + b)";
  unit_test ((subst "x"  (Var("y")) (Binop(Plus, Var("x"), Var("x")))) = Binop(Plus, Var("y"), Var("y"))) "subst x y (x + x)";
  unit_test ((subst "x"  (Var("y")) (Binop(Minus, Binop(Plus, Num(5), Var("x")), Var("x")))) = Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("y"))) "subst x y (5*x - x)";
  unit_test ((subst "x"  (Var("y")) (Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("x")), Var("x"))))) = Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("x")), Var("x")))) "subst x y (fun x -> 5*x - x)";
  unit_test ((subst "x"  (Var("z")) (Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("y"))))) = Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("y")))) "subst x z (fun x -> 5*y - y)";
  unit_test ((subst "x"  (Var("z")) (Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("x"))))) = Fun("x", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("x")))) "subst x z (fun x -> 5*y - x)";
  unit_test ((subst "x"  (Var("z")) (Fun("a", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("x"))))) = Fun("a", Binop(Minus, Binop(Plus, Num(5), Var("y")), Var("z")))) "subst x z (fun a -> 5*y - x)";
  unit_test ((subst "x" (Num(8)) (Fun("y", Var("x")))) = Fun("y", Num(8))) "subst x 8 (fun y -> x)";
  unit_test ((subst "x" (Var("y")) (Fun("y", Var("x")))) = Fun("v2", Var("y"))) "subst x y (fun y -> x)";
  unit_test ((subst "x" (Var("y")) (Fun("y", Binop(Plus, Var("x"), Var("y"))))) = Fun("v3", Binop(Plus, Var("y"), Var("v3")))) "subst x y (fun y -> x + y)";
  unit_test ((subst "x" (Var("y")) (Let("x", Num(8), Var("x")))) = Let("x", Num(8), Var("x"))) "subst x y (let x = 8 in x)";
  unit_test ((subst "x" (Var("y")) (Let("x", Var("x"), Var("x")))) = Let("x", Var("y"), Var("x"))) "subst x y (let x = x in x)";
  unit_test ((subst "x" (Var("m")) (Let("y", Var("x"), Var("x")))) = Let("y", Var("m"), Var("m"))) "subst x m (let y = x in x)";
  unit_test ((subst "x" (Var("y")) (Let("y", Var("x"), Var("y")))) = Let("v4", Var("y"), Var("v4"))) "subst x y (let y = x in y)";
  unit_test ((subst "x" (Var("y")) (Let("y", Var("x"), Binop(Plus, Var("y"), Var("x"))))) = Let("v5", Var("y"), Binop(Plus, Var("v5"), Var("y")))) "subst x y (let y = x in y + x)";
  unit_test ((subst "x" (Var("y")) (Letrec("x", Binop(Plus, Var("x"), Num(8)), Var("x")))) = Letrec("x", Binop(Plus, Var("x"), Num(8)), Var("x"))) "subst x y (let rec x = x + 8 in x)";
  unit_test ((subst "x" (Var("m")) (Letrec("y", Binop(Plus, Var("x"), Var("y")), Var("x")))) = Letrec("y", Binop(Plus, Var("m"), Var("y")), Var("m"))) "subst x m (let rec y = x + y in x)";
  unit_test ((subst "x" (Var("y")) (Letrec("y", Binop(Plus, Var("x"), Var("y")), Var("y")))) = Letrec("v6", Binop(Plus, Var("y"), Var("v6")), Var("v6"))) "subst x y (let rec y = x + y in y)";
  unit_test ((subst "x" (Var("y")) (Letrec("y", Binop(Plus, Var("x"), Var("y")), Binop(Plus, Var("x"), Var("y"))))) = Letrec("v7", Binop(Plus,Var("y"),Var("v7")), Binop(Plus,Var("y"), Var("v7")) )) "subst x y (let rec y = x + y in x + y)";
  unit_test ((subst "x" (Var("y")) (App(Fun("x", Binop(Plus, Var("x"), Num(1))), Num(5)))) = App(Fun("x", Binop(Plus, Var("x"), Num(1))), Num(5))) "subst x y ((fun x -> x + 1) 5)";
  unit_test ((subst "x" (Var("y")) (App(Fun("a", Binop(Plus, Var("x"), Num(1))), Num(5)))) = App(Fun("a", Binop(Plus, Var("y"), Num(1))), Num(5))) "subst x y ((fun a -> x + 1) 5)";
  unit_test ((subst "x" (Var("y")) (App(Fun("a", Binop(Plus, Var("x"), Num(1))), Var("x")))) = App(Fun("a", Binop(Plus, Var("y"), Num(1))), Var("y"))) "subst x y ((fun a -> x + 1) x)";
  unit_test ((subst "x" (Var("y")) (Conditional(Bool(true), Var("x"), Var("y")))) = Conditional(Bool(true), Var("y"), Var("y"))) "subst x y if true then x else y";
  unit_test ((subst "x" (Var("y")) (Conditional(Binop(Equals, Var("x"), Var("y")), Var("x"), Var("y")))) = Conditional(Binop(Equals, Var("y"), Var("y")), Var("y"), Var("y"))) "subst x y if x = y then x else y";
  unit_test ((subst "x" (Var("y")) (Conditional(Bool(true), Num(5), Num(4)))) = Conditional(Bool(true), Num(5), Num(4))) "subst x y if true then 5 else 4";
  
  () ;;

test ();;
