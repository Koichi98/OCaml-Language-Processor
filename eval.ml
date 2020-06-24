open Syntax
open Match

let empty_env = []
let extend x v env = (x,v)::env

exception EvalErr

let rec lookup x env =
  try List.assoc x env with Not_found -> Printf.printf "Exception: EvalError %s UNFOUND" x;raise EvalErr

let rec eval_expr env e = 
  match e with
|EConstInt i -> 
  VInt i
|EConstBool b ->
  VBool b
|EVar x ->
  let Thunk(e',env') = lookup x env in
  eval_expr !env' e'
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
    eval_expr ((n,Thunk(e1,ref env))::env) e2
| ENot e ->  (*否定演算子の評価*)
    (match eval_expr env e with
    |VBool b -> VBool (not b)
    |_ -> Printf.printf "Exception: EvalError NOT";raise EvalErr)
| EFun (n,e) -> VFun(n,e,ref env) (*関数をクロージャとしてvalue型に変換*)
| EApp (e1,e2)-> (*関数の適用*)
  let v1 = eval_expr env e1 in
  let thunk2 = Thunk(e2,ref env) in
  (match v1 with
    |VFun(n,e,oenv) ->
       eval_expr (extend n thunk2 !oenv) e
    (*|VRecFun (i,ls,oenv) -> 
      let (f,x,e) = fun_i i ls in
        let env' = extend x v2 (extend_list 1 ls ls oenv) in 
        eval_expr env' e*)
    |_ -> Printf.printf "Exception: EvalError FUNCTION APPLY ";raise EvalErr)
(*| ELetRec (ls,e) ->
    let env' = 
      extend_list 1 ls ls env
    in eval_expr env' e *)
| EPair (e1,e2) -> (*組型への拡張*)
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  VPair(v1,v2)
| ENil -> VNil (*空リスト*)
| ECons (e1,e2) -> 
  let v1 = eval_expr env e1 in
  VCons (v1,(eval_expr env e2))
| ELetRec ((f,x,e1)::rest,e2)-> (*相互再帰を考慮しないのでrest=[]を想定している*)
    let oenv = ref [] in 
    let thunk = Thunk(EFun(x,e1),oenv) in
    oenv := ((f,thunk)::env);
    eval_expr !oenv e2

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
    |value -> ("val", (n,Thunk(e,ref env))::env,value))
  | CRecDecl((f,x,e)::rest) -> 
    let oenv = ref [] in 
    let thunk = Thunk(EFun(x,e),oenv) in
    oenv := ((f,thunk)::env);
    ("val", !oenv , VFun(f,e,oenv)) (*再帰関数クロージャを環境追加して関数を定義*)
  | PLErr -> Printf.printf "Exception: Parse or Lex Error";("Error" , env, VErr "Error")

