(*
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions
 *)

type unop =
  | Negate
;;

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
  | Unit                                 (* for returning nothing *)
;;

(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;

(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var x -> SS.singleton x
  | Num _ | Bool _ | Unit | Unassigned | Raise -> SS.empty
  | Unop (_, arg) -> free_vars arg
  | Binop (_, arg1, arg2) ->
    SS.union (free_vars arg1) (free_vars arg2)
  | Conditional (a, arg1, arg2) ->
    SS.union (free_vars a) (SS.union (free_vars arg1) (free_vars arg2))
  | Fun (x, def) -> SS.remove x (free_vars def)
  | Let (x, def, body) ->
    SS.union (free_vars def) (SS.remove x (free_vars body))
  | Letrec (x, def, body) ->
    SS.union (SS.remove x (free_vars def)) (SS.remove x (free_vars body))
  | App (arg1, arg2) -> SS.union (free_vars arg1) (free_vars arg2) ;;

(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname : unit -> varid =
  let i = ref 0 in
  fun () ->
  i := !i + 1;
  "v" ^ string_of_int (!i - 1) ;;

(*......................................................................
  Substitution

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let frees = free_vars repl in
  match exp with
  | Var x -> if x = var_name then repl else Var x
  | Num _  | Bool _ | Unassigned | Raise | Unit -> exp
  | Unop (o, arg) -> Unop (o, subst var_name repl arg)
  | Binop (o, arg1, arg2) ->
    Binop (o, subst var_name repl arg1, subst var_name repl arg2)
  | Conditional (a, arg1, arg2) ->
    Conditional (subst var_name repl a, subst var_name repl arg1,
                 subst var_name repl arg2)
  | App (arg1, arg2) -> App (subst var_name repl arg1, subst var_name repl arg2)
  | Fun (x, def) ->
    if var_name = x then exp
    else if SS.mem x frees then let new_v = new_varname () in
    Fun (new_v, subst var_name repl (subst x (Var new_v) def))
    else Fun (x, subst var_name repl def)
  | Let (x, def, body) ->
    if var_name = x then Let (x, subst var_name repl def, body)
    else if SS.mem x frees then let new_v = new_varname () in
      Let (new_v, subst var_name repl def,
           subst var_name repl (subst x (Var new_v) body))
    else Let (x, subst var_name repl def, subst var_name repl body)
  | Letrec (x, def, body) ->
    if var_name = x then exp
    else if SS.mem x frees then let new_v = new_varname () in
      Letrec (new_v, subst var_name repl (subst x (Var new_v) def),
           subst var_name repl (subst x (Var new_v) body))
    else
      Letrec (x, subst var_name repl def, subst var_name repl body) ;;

(*......................................................................
  String representations of expressions
 *)


(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var x -> x
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unop (op, arg) ->
    (match op with
     | Negate -> "not " ^ exp_to_concrete_string arg)
  | Binop (op, arg1, arg2) ->
    let make_str op_s = exp_to_concrete_string arg1 ^ op_s ^
                        exp_to_concrete_string arg2 in
    (match op with
      | Plus -> make_str " + "
      | Minus -> make_str " - "
      | Times -> make_str " * "
      | Equals -> make_str " = "
      | LessThan -> make_str " < ")
  | Conditional (a, arg1, arg2) ->
    "if " ^ (exp_to_concrete_string a) ^ " then " ^
    (exp_to_concrete_string arg1) ^ " else " ^ (exp_to_concrete_string arg2)
  | Fun (x, def) -> "fun " ^ x ^ " -> " ^ exp_to_concrete_string def
  | Let (x, def, body) ->
    "let " ^ x ^ " = " ^ exp_to_concrete_string def ^ " in " ^
    exp_to_concrete_string body
  | Letrec (x, def, body) ->
    "let rec " ^ x ^ " = " ^ exp_to_concrete_string def ^ " in " ^
    exp_to_concrete_string body
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (arg1, arg2) ->
    exp_to_concrete_string arg1 ^ " " ^ exp_to_concrete_string arg2
  | Unit -> "()" ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ ")"
  | Num n -> "Num(" ^ string_of_int n ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop (op, arg) ->
    (match op with
     | Negate -> "Unop(Negate, " ^ exp_to_abstract_string arg) ^ ")"
  | Binop (op, arg1, arg2) ->
    let make_str op_s = "Binop(" ^ op_s ^ ", " ^  exp_to_abstract_string arg1 ^
                        ", " ^ exp_to_abstract_string arg2 ^ ")" in
    (match op with
      | Plus -> make_str "Plus"
      | Minus -> make_str "Minus"
      | Times -> make_str "Times"
      | Equals -> make_str "Equals"
      | LessThan -> make_str "LessThan")
  | Conditional (a, arg1, arg2) ->
    "Conditional(" ^ (exp_to_abstract_string a) ^ ", " ^
    (exp_to_abstract_string arg1) ^ ", " ^ (exp_to_abstract_string arg2) ^ ")"
  | Fun (x, def) -> "Fun(" ^ x ^ ", " ^ exp_to_abstract_string def ^ ")"
  | Let (x, def, body) ->
    "Let(" ^ x ^ ", " ^ exp_to_abstract_string def ^ ", " ^
    exp_to_abstract_string body ^ ")"
  | Letrec (x, def, body) ->
    "Letrec(" ^ x ^ ", " ^ exp_to_abstract_string def ^ ", " ^
    exp_to_abstract_string body ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (arg1, arg2) ->
    "App(" ^ exp_to_abstract_string arg1 ^ ", " ^ exp_to_abstract_string arg2 ^
    ")"
  | Unit -> "Unit" ;;
