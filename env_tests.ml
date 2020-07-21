open Test_simple;;
open Expr;;
open Evaluation;;

let e = Env.create ();;
let e1 = Env.extend e "y" (ref (Env.Val (Num(5))));;
let e2 = Env.extend e1 "z" (ref (Env.Val (Num(7))));;
let e3 = Env.extend e "f" (ref (Env.Closure (Fun("x", Num(10)), e)));;
let e4 = Env.extend e "f" (ref (Env.Closure (Fun("x", Num(10)), e1)));;


let test () =
  (* close *)
  unit_test (Env.close (Num(5)) e = Env.Closure (Num(5), e)) "close 5 env []";
  unit_test (Env.close (Fun("x", Num(7))) e1 = Env.Closure (Fun("x", Num(7)), e1)) "close (fun x -> 7) env y = 5";
  unit_test (Env.close (Fun("x", Var("y"))) e1 = Env.Closure (Fun("x", Var("y")), e1)) "close (fun x -> y) env y = 5";

  (* lookup and extend *)
  unit_test (try Env.lookup e "y" = Env.Val (Bool(false))
             with EvalError ("unbound variable y") -> true) "lookup y env []";
  unit_test (Env.lookup e1 "y" = Env.Val (Num(5))) "lookup y env y = 5";
  unit_test (Env.lookup e3 "f" = Env.Closure (Fun("x", Num(10)), e)) "lookup f env f = Closure (fun x -> 10), e";
  unit_test (Env.lookup e2 "z" = Env.Val (Num(7))) "lookup z env y = 5, z = 7";
  unit_test (Env.lookup e2 "y" = Env.Val (Num(5))) "lookup y env y = 5, z = 7";
  unit_test (Env.lookup (Env.extend e2 "y" (ref (Env.Val (Num(8))))) "y" = Env.Val (Num(8))) "lookup y extend (env y = 5, z = 7) to y = 8";
  unit_test (Env.lookup e2 "y" = Env.Val (Num(5))) "lookup y env y = 5, z = 7";

  (* env_to_string *)
  unit_test (Env.env_to_string e = "[]") "env_to_s env []";
  unit_test (Env.env_to_string e1 = "[y : Value Num(5), ]") "env_to_s env y = 5";
  unit_test (Env.env_to_string e2 = "[z : Value Num(7), y : Value Num(5), ]") "env_to_s env y = 5 z = 7";
  unit_test (Env.env_to_string e4 = "[f : Closure Fun(x, Num(10)), Environment: [y : Value Num(5), ], ]") "env f = Closure (fun x -> 10), e1";

  (* value_to_string (tested also in env_to_string tests) *)
  unit_test (Env.value_to_string (Env.Val (Num(5))) = "Value Num(5)") "val_to_s 5" ;
  unit_test (Env.value_to_string (Env.Closure (Num(5), e)) = "Closure Num(5), Environment: []") "val_to_s closure w env" ;
  unit_test (Env.value_to_string (Env.Closure (Num(5), e1)) = "Closure Num(5), Environment: [y : Value Num(5), ]") "val_to_s closure w env2" ;
  unit_test (Env.value_to_string ~printenvp:false (Env.Closure (Num(5), e)) = "Closure Num(5)") "val_to_s closure w/o env" ;

  ();;

test ();;
