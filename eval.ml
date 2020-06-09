open Syntax

exception Unbound

type env = (name * value) list

let empty_env = []
let extend x v env = (x,v)::env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

let rec eval_expr env e = 
  match e with
|EConstInt i -> 
  VInt i
|EConstBool b ->
  VBool b
|EVar x ->
(try
  lookup x env 
  with 
  |Unbound -> Printf.printf "Exception: EvalError %s UNFOUND" x;raise EvalErr)
|EAdd (e1,e2) ->
  let v1 = eval_expr env e1 in 
  let v2 = eval_expr env e2 in
  (match v1, v2 with
  |VInt i1, VInt i2 -> VInt (i1+i2)
  |_ -> Printf.printf "Exception: EvalError ADDITION";raise EvalErr)
|ESub (e1,e2) -> (*加算と同様に減算の場合も処理*)
  let v1 = eval_expr env e1 in 
  let v2 = eval_expr env e2 in
  (match v1, v2 with
  |VInt i1, VInt i2 -> VInt (i1-i2)
  |_ -> Printf.printf "Exception: EvalError SUBTRACTION" ;raise EvalErr)
|EMul (e1,e2) ->(*加算と同様の乗算の場合も処理*)
  let v1 = eval_expr env e1 in 
  let v2 = eval_expr env e2 in
  (match v1, v2 with
  |VInt i1, VInt i2 -> VInt (i1*i2)
  |_ -> Printf.printf "Exception: EvalError MULTIPLICATION";raise EvalErr)
|EDiv (e1,e2) ->(*加算と同様に除算の場合も処理*)
  let v1 = eval_expr env e1 in 
  let v2 = eval_expr env e2 in
  (match v1, v2 with
  |VInt i1, VInt i2 -> VInt (i1/i2)
  |_ -> Printf.printf "Exception: EvalError DIVISION";raise EvalErr)
|EOr (e1,e2) ->(*論理和を定義*)
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  (match v1, v2 with
  |VBool b1, VBool b2 ->VBool (b1||b2)
  |_ -> Printf.printf "Exception: EvalError OR";raise EvalErr)
|EAnd (e1,e2) ->(*論理積を定義*)
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  (match v1, v2 with
  |VBool b1, VBool b2 ->VBool (b1&&b2)
  |_ -> Printf.printf "Exception: EvalError AND";raise EvalErr)
| EEq (e1,e2) ->
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  (match v1, v2 with
    | VInt i1,  VInt i2  -> VBool (i1 = i2)
    | _ -> Printf.printf "Exception: EvalError EQUAL";raise EvalErr)
| ELt (e1,e2) ->
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  (match v1, v2 with
    | VInt i1,  VInt i2  -> VBool (i1 < i2)
    | _ -> Printf.printf "Exception: EvalError LESS THAN";raise EvalErr)
| EIf (e1,e2,e3) ->
  let v1 = eval_expr env e1 in
  (match v1 with
    | VBool b ->
      if b then eval_expr env e2 else eval_expr env e3
    | _ -> Printf.printf "Exception: EvalError IF";raise EvalErr)
| ELet (n,e1,e2) -> (*更新したenvのもとでe2を評価する*)
    eval_expr ((n, eval_expr env e1)::env) e2
| ENot e ->  (*否定演算子の評価*)
    (match eval_expr env e with
    |VBool b -> VBool (not b)
    |_ -> Printf.printf "Exception: EvalError NOT";raise EvalErr)
  

let eval_expr_with_err env e = (*EvalErrが起きたときにそれを拾う関数*)
  try 
    eval_expr env e
    with 
     | EvalErr -> VErr "Error"


let rec declare n e env =  (*連続での宣言に対応した関数*)
match e with 
| EAnoDecl (e1,newname,e2) ->
  (match eval_expr_with_err env e1 with 
  | VErr a-> raise EvalErr
  | VInt i -> Printf.printf "val %s : int = " n;print_int i;print_newline ();declare newname e2 (extend n (VInt i) env)
  | VBool b -> Printf.printf "val %s : bool = " n;print_string (string_of_bool b);print_newline ();declare newname e2 (extend n (VBool b) env))
| _ -> 
  match eval_expr_with_err env e with 
  | VErr a -> raise EvalErr 
  | VInt i -> Printf.printf "val %s : int = " n;print_int i;extend n (VInt i) env
  | VBool b -> Printf.printf "val %s : bool = " n;print_string (string_of_bool b);extend n (VBool b) env


let declare_with_err n e env = (*EvalErrが起きたときにそれを拾う関数*)
  try 
    declare n e env
    with 
      EvalErr -> [("Error", VInt 0)] (*正常に動いたとき(name,value)のリストを返すので,エラーの場合も合わせる.*)


let rec eval_command env c =
  match c with
  | CExp e -> 
  (match eval_expr_with_err env e with 
    |VErr a -> (a , env, VErr a) (*返り値の不一致によるエラーを防ぐため(string,env,value)の三つ組みを返している*)
    |value -> ("-", env,value))
  | CDecl (n,e) ->  (*入力がトップレベル定義だった場合*)
  (match declare_with_err n e env with
    |[("Error",VInt 0)] -> ("Error", env, VErr "Error") (*返り値の不一致によるエラーを防ぐため(string,env,value)の三つ組みを返している*)
    |env -> ("val",env,VInt 0))
  | PLErr -> Printf.printf "Exception: Parse/Lex Error";("Error" , env, VErr "Error")

  