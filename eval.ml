open Syntax
open Match


exception Unbound

(*type env = (name * value) list*)

let empty_env = []
let extend x v env = (x,v)::env

let rec fun_i i ls = 
  let x::xs = ls in
  if i=1 then x else fun_i (i-1) xs

let rec extend_list i ls rest env=
  match rest with
|[] -> env
|(f,x,e)::xs -> (f,VRecFun(i,ls,env))::(extend_list (i+1) ls xs env)

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
| EFun (n,e) -> VFun (n,e,env) (*関数をクロージャとしてvalue型に変換*)
| EApp (e1,e2)-> (*関数の適用*)
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  (match v1 with
    |VFun(x,e,oenv)->
       eval_expr (extend x v2 oenv) e
    |VRecFun (i,ls,oenv) -> 
      let (f,x,e) = fun_i i ls in
        let env' = extend x v2 (extend_list 1 ls ls oenv) in 
        eval_expr env' e
    |_ -> Printf.printf "Exception: EvalError FUNCTION APPLY ";raise EvalErr)
| ELetRec (ls,e) ->
    let env' = 
      extend_list 1 ls ls env
    in eval_expr env' e 
| EPair (e1,e2) -> (*組型への拡張*)
  let v1 = eval_expr env e1 in
  let v2 = eval_expr env e2 in
  VPair(v1,v2)
| ENil -> VNil (*空リスト*)
| ECons (e1,e2) -> 
  let v1 = eval_expr env e1 in
  VCons (v1,(eval_expr env e2))
| EMatch (e,p_list) -> 
  (find_match_sub p_list (eval_expr env e) env)
and  
find_match_sub p_list value env =
  match p_list with 
  |[] -> Printf.printf "Exception: EvalError PATTERN MATCH";raise EvalErr
  |(p,e)::rest -> 
    (match find_match p value with
    |None -> find_match_sub rest value env
    |Some ls -> eval_expr (ls@env) e)


    (*|VRecFun (f,x,e,oenv) -> 
      let env' =
        extend x v2 (extend f (VRecFun(f,x,e,oenv)) oenv) 
        in eval_expr env' e*)
(*| ELetRec (f,x,e1,e2)-> (*再帰関数クロージャの環境に追加して評価*)
    let env' = 
      extend f (VRecFun(f,x,e1,env)) env
      in 
      eval_expr env' e2*)
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
    |value -> ("val", extend n value env,value))
  (*| CRecDecl (f,x,e) -> ("val", extend f (VRecFun(f,x,e,env)) env,(VRecFun(f,x,e,env))) (*再帰関数クロージャを環境追加して関数を定義*)*)
  | CRecDecl ls -> ("val", extend_list 1 ls ls env, VRecFun(0,ls,env)) (*再帰関数クロージャを環境追加して関数を定義*)
  | PLErr -> Printf.printf "Exception: Parse or Lex Error";("Error" , env, VErr "Error")


(*)  |CDecl (n,e) -> infer_expr 
  |CRecDecl *)



    
    

(*let rec infer_cmd cmd =*)

(*let subst3 = unify [((TyVar alpha),(TyFun((TyVar t),(TyVar beta))));((TyVar alpha'),(TyFun((TyVar alpha''),(TyVar t))))]
print_type (TyFun ((ty_subst subst3 (TyVar alpha)),(TyFun ((ty_subst subst3 (TyVar alpha')),(TyFun ((ty_subst subst3 (TyVar alpha'')),(ty_subst subst3 (TyVar beta))))))))
print_type (TyFun((ty_subst subst3 (TyVar alpha)),(ty_subst subst3 (TyVar beta))))*)