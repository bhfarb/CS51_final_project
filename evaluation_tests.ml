open Test_simple ;;      (* a really simple unit testing framework *)
open Evaluation ;;
open Expr ;;

let e = Env.create ();;
let e1 = Env.extend e "y" (ref (Env.Val (Num(5))));;
let e2 = Env.extend e1 "z" (ref (Env.Val (Num(7))));;
let e3 = Env.extend e "f" (ref (Env.Closure (Fun("x", Num(10)), e)));;

let test (eval : expr -> Env.env -> Env.value) (name : string) (() : unit) : unit =
  (* num *)
  unit_test (eval (Num(1)) e = Env.Val (Num(1))) (name ^ " 1 env []");

  (* bool *)
  unit_test (eval (Bool(true)) e = Env.Val (Bool(true))) (name ^ " true, env []");

  (* var *)
  unit_test (try eval (Var("x")) e1 = Env.Val (Num(5))
             with | EvalError "var: type error" -> name = "s"
                  | EvalError "unbound variable x" -> name = "l" || name = "d") (name ^ " x, env y = 5");
  unit_test (try eval (Var("y")) e1 = Env.Val (Num(5))
             with EvalError "var: type error" -> name = "s") (name ^ " y, env y = 5");

  (* unop *)
  unit_test (eval (Unop(Negate, Num(1))) e = Env.Val (Num(-1))) (name ^ " (~-) 1, env []");
  unit_test (try eval (Unop(Negate, Var("y"))) e1 =  Env.Val (Num(-5))
             with EvalError "var: type error" -> name = "s") (name ^ " (~-) y, env y = 5");
  unit_test (try eval (Unop(Negate, Bool(true))) e =  Env.Val (Bool(false))
             with EvalError "negate: type error" -> true) (name ^ " (~-) true, env []");

  (* binop *)
  unit_test (eval (Binop(Plus, Num(1), Num(2))) e1 = Env.Val (Num(3))) (name ^ " 1 + 2, env y = 5");
  unit_test (try eval (Binop(Plus, Var("y"), Num(2))) e1 = Env.Val (Num(7))
             with EvalError "var: type error" -> name = "s") (name ^ " y + 2, env y = 5");

  (* conditional *)
  unit_test (eval (Conditional(Bool(true), Num(8), Num(1))) e = Env.Val (Num(8))) (name ^ " if true then 8 else 1 env []");
  unit_test (eval (Conditional(Bool(false), Num(8), Num(1))) e = Env.Val (Num(1))) (name ^ " if false then 8 else 1 env []");


  (* fun *)
  unit_test (eval (Fun("x", Var("x"))) e =
             if name = "l" then Env.Closure (Fun("x", Var("x")), e)
             else Env.Val (Fun("x", Var("x")))) (name ^ " fun x -> x");
  unit_test (eval (Fun("x", Var("y"))) e1 =
             if name = "l" then Env.Closure (Fun("x", Var("y")), e1)
             else Env.Val (Fun("x", Var("y")))) (name ^ " fun x -> y, env y = 5");

  (* let *)
  unit_test (eval (Let("y", Num(5), Var("y"))) e = Env.Val (Num(5))) (name ^ " let y = 5 in y, env []");
  unit_test (try eval (Let("x", Var("y"), Var("x"))) e1 = Env.Val (Num(5))
             with EvalError "var: type error" -> name = "s" ) (name ^ " let x = y in x, env y = 5");
  unit_test (try eval (Let("x", Var("y"), Binop(Equals, Var("x"), Var("y")))) e1 =  Env.Val (Bool(true))
             with EvalError "var: type error" -> name = "s") (name ^ " let x = y in (x = y), env y = 5");
  unit_test (eval (Let("y", Num(3), Binop(Equals, Var("y"), Num(3)))) e1 =  Env.Val (Bool(true))) (name ^ " let y = 3 in (y = 3), env y = 5");

  (* let rec *)
  unit_test (try eval (Letrec("x", Binop(Plus, Var("x"), Num(8)), Var("x"))) e = Env.Val (Bool(true))
             with | EvalError "var: type error" -> name = "s"
                  | EvalError "plus: type error" -> name = "d" || name = "l") " let rec x = x + 8 in x env []";
  unit_test (eval (Letrec("fact", Fun("n", Conditional(Binop(Equals, Var("n"), Num(0)), Num(1), Binop(Times, Var("n"), App (Var("fact"),Binop(Minus, Var("n"), Num(1)))))), App(Var("fact"), Num(4)))) e
             = Env.Val (Num(24))) (name ^ " factorial 4 env ");
  unit_test (eval (Letrec("fact", Fun("n", Conditional(Binop(Equals, Var("n"), Num(0)), Num(1), Binop(Times, Var("n"), App (Var("fact"),Binop(Minus, Var("n"), Num(1)))))), App(Var("fact"), Num(2)))) e
             = Env.Val (Num(2))) (name ^ " factorial 2 env ");
  unit_test (eval (Letrec("fact", Fun("n", Conditional(Binop(Equals, Var("n"), Num(0)), Num(1), Binop(Times, Var("n"), App (Var("fact"),Binop(Minus, Var("n"), Num(1)))))), App(Var("fact"), Num(2)))) e
             = Env.Val (Num(2))) (name ^ " factorial 2 env ");
  unit_test (eval (Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1))))), App(Var("f"), Num(2)))) e
             = Env.Val (Num(0))) (name ^ " let rec f = fun x -> if x = 0 then x else f (x - 1) in f 2 env []");
  unit_test (try eval (Letrec("x", Var("x"), Var("x"))) e = Env.Val (Bool(false))
             with | EvalError "var: type error" -> name = "s"
                  | EvalError "recursive loop" -> name = "l" || name = "d") (name ^ " let rec x = x in x env []");

  (* app *)
  unit_test (try eval (App(Fun("x", Var("y")), Num(2))) e1 = Env.Val (Num(5))
             with EvalError "var: type error" -> name = "s") (name ^ " (fun x -> y) 2, env y = 5");
  unit_test (eval (Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))) , Let("x", Num(2), App(Var("f"), Num(3)))))) e =
             if name = "d" then Env.Val (Num(5)) else Env.Val(Num(4))) (name ^ " reassignment env []");
  unit_test (try eval (App(Var("x"), Num(2))) e = Env.Val (Bool(false))
             with | EvalError "var: type error" -> name = "s"
                  | EvalError "unbound variable x" -> name = "l" || name = "d") (name ^ " x 2 env []");
  unit_test (try eval (App(Var("f"), Num(2))) e3 = Env.Val (Num(10))
             with | EvalError "var: type error" -> name = "s"
                  | EvalError "app: type error" -> name = "d") (name ^ " f 2 env [Closure f, fun x -> 10]");

  (* raise *)
  unit_test (try eval Raise e = Env.Val (Bool(false)) with EvalException -> true) (name ^ " raise env []");

  (* unassigned *)
  unit_test (try eval Unassigned e = Env.Val (Bool(false)) with EvalError "unassigned" -> true) (name ^ " unassigned env []");

  (* unit *)
  unit_test (eval Unit e = Env.Val (Unit) ) (name ^ " unit env []");
  unit_test (eval (Let("f", Fun ("x", Unit), App(Var("f"), Num(3)))) e = Env.Val (Unit) ) (name ^ " let f = fun x -> () in f 3 env []");
  unit_test (eval (Let("f", Fun ("()", Num(4)), App(Var("f"), Unit))) e = Env.Val (Num(4)) ) (name ^ " let f = fun () -> 4 in f () env []");

  () ;;

test eval_s "s" () ;;
test eval_d "d" () ;;
test eval_l "l" () ;;
