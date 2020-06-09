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
| EFun(x,e) -> VFun(x,e,ref env)
| EApp (e1,e2) -> 
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in 
  (match v1 with 
  |VFun(x,e,oenv)-> 
  eval_expr (extend x v2 (!oenv)) e)
| ELetRec (f,x,e1,e2) ->
    let oenv = ref [] in
    let v = VFun(x, e1, oenv) in
    (oenv := extend f v env;
    eval_expr (extend f v env) e2)


let eval_expr_with_exp env e = (*EvalErrが起きたときにそれを拾う関数*)
  try 
    eval_expr env e
    with 
     | EvalErr -> VErr "Error"


let rec eval_command env c =
  match c with
  | CExp e -> 
  (match eval_expr_with_exp env e with 
    |VErr a -> (a , env, VErr a) (*返り値の不一致によるエラーを防ぐため(string,env,value)の三つ組みを返している*)
    |value -> ("-", env,value))
  | CDecl (n,e) ->  (*入力がトップレベル定義だった場合*)
  (match eval_expr_with_exp env e with 
    |VErr a -> (a , env, VErr a)
    |VFun(x,e,oenv) -> ("val",extend n (VFun(x,e,oenv)) env,VFun(n,e,oenv))
    |value -> ("val", extend n value env,value))
  | CRecDecl (f,x,e) ->  let oenv = ref [] in
                            let v = VFun(x,e,oenv) in
                              (oenv:= extend f v env;("val",extend f v env,VFun(f,e,oenv)))


  