open Syntax
open TySyntax
open ConstraintSolver 
open Polymorphism

type tyenv = (name*tyscheme) list (*型スキームを用いた型環境*)

exception Unbound

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Unbound

let rec infer_expr tyenv expr = (*型制約を生成する*)
  match expr with 
  |EConstInt i -> (TyInt,[])
  |EConstBool b -> (TyBool,[])
  |EVar x -> 
    let t = instantiate (lookup x tyenv) in
    (t,[])
  |EAdd(e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  |ESub(e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  |EMul(e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  |EDiv(e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyInt,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  |EAnd(e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyBool,[(t1,TyBool);(t2,TyBool)]@c1@c2)
  |EOr(e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyBool,[(t1,TyBool);(t2,TyBool)]@c1@c2)
  |EEq(e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyBool,[(t1,TyInt);(t2,TyInt)]@c1@c2)
  |ELt(e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    ((TyFun((TyFun(TyInt,TyInt)),TyBool)),[(t1,TyInt);(t2,TyInt)]@c1@c2)
  |ELet(n,e1,e2) -> 
    let (t1,c1) = infer_expr tyenv e1 in 
    let sigma = unify c1 in 
    let s1 = ty_subst sigma t1 in
    let delta = update_tyenv sigma tyenv in
    let (p,t) = generalize delta s1 in
    let (t2,c2) = infer_expr ((n,(p,t))::tyenv) e2 in 
    (t2,c1@c2)
  |ENot e ->
    let (t,c) = infer_expr tyenv e in
    (TyBool,(t,TyBool)::c)
  |EIf(e1,e2,e3) -> 
    let (t1,c1) = infer_expr tyenv e1 in 
    let (t2,c2) = infer_expr tyenv e2 in 
    let (t3,c3) = infer_expr tyenv e3 in 
    (t2,[(t1,TyBool);(t2,t3)]@c1@c2@c3)
  |EFun(n,e) -> 
    let alpha = TySyntax.new_tyvar () in 
    let (t,c) = infer_expr ((n,([],(TyVar alpha)))::tyenv) e in 
    ((TyFun((TyVar alpha),t)),c)
  |EApp(e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in 
    let (t2,c2) = infer_expr tyenv e2 in 
    let alpha = TySyntax.new_tyvar () in 
    ((TyVar alpha),[(t1,(TyFun(t2,(TyVar alpha))))]@c1@c2)
  |ELetRec((f,x,e1)::rest,e2) ->(*相互再帰は想定していないのでrest=[]を仮定している*)
    let alpha = TySyntax.new_tyvar () in 
    let beta = TySyntax.new_tyvar () in 
    let (t1,c1) = infer_expr ([(f,([],(TyFun((TyVar alpha),(TyVar beta)))));(x,([],(TyVar alpha)))]@tyenv) e1 in
    let sigma = unify ([(t1,(TyVar beta))]@c1) in 
    let s1 = ty_subst sigma (TyFun((TyVar alpha),t1)) in
    let delta = update_tyenv sigma tyenv in
    let (p,t) = generalize delta s1 in
    let (t2,c2) = infer_expr ((f,(p,t))::tyenv) e2 in
    (t2,[(t1,(TyVar beta))]@c1@c2)

let infer_cmd tyenv cmd = (*infer_exprで得た型制約から,unifyを用いて型推論を実際におこなう*)
  match cmd with
  |CExp e -> 
    let (t,c) = infer_expr tyenv e in
    (*print_type_list c;*) (*デバッグ用,生成された型制約の出力*)
    (ty_subst (unify c) t,tyenv)
  |CDecl (n,e) ->
    let (t,c) = infer_expr tyenv e in
    let sigma = unify c in 
    let s = ty_subst sigma t in
    let delta = update_tyenv sigma tyenv in
    let (p,t) = generalize delta s in
    (t,((n,(p,t))::tyenv))
  |CRecDecl ((f,x,e)::rest) ->
    let alpha = TySyntax.new_tyvar () in 
    let beta = TySyntax.new_tyvar () in 
    let (t1,c1) = infer_expr ([(f,([],(TyFun((TyVar alpha),(TyVar beta)))));(x,([],(TyVar alpha)))]@tyenv) e in
    let sigma = unify ([(t1,(TyVar beta))]@c1) in 
    let s1 = ty_subst sigma (TyFun((TyVar alpha),t1)) in
    let delta = update_tyenv sigma tyenv in
    let (p,t) = generalize delta s1 in
    (t,((f,(p,t))::tyenv))

