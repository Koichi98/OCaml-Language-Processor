open TySyntax
exception TyError

type subst = (tyvar * ty) list
type constraints = (ty * ty) list




let rec sub2_compose tyv1 t1 t = (*sub_composeの補助関数,深さ優先探索で置き換えを行う*)
  match t with 
  |TyInt -> TyInt
  |TyBool -> TyBool
  |TyFun(ty1,ty2) ->
    TyFun(sub2_compose tyv1 t1 ty1,sub2_compose tyv1 t1 ty2)
  |TyVar tyv -> 
    if tyv = tyv1 then t1 else TyVar tyv
  |TyPair (ty1,ty2) -> 
    TyPair(sub2_compose tyv1 t1 ty1,sub2_compose tyv1 t1 ty2)
  |TyNil -> TyNil
  |TyCons ty -> 
    TyCons(sub2_compose tyv1 t1 ty) 


let rec sub_compose tyv1 t1 sigma2 = (*sigma2内の代入する型の中で,tyv1があればそれをt1に置き換える*)
  match sigma2 with
  |[] -> []
  |(tyv2,t2)::rest ->
    (tyv2,(sub2_compose tyv1 t1 t2))::(sub_compose tyv1 t1 rest)


let rec compose sigma1 sigma2 = (*代入の合成を行う関数 σ1(σ2)*)
  match sigma1 with 
  |[] -> sigma2
  |(tyv1,t1)::rest ->
    (compose rest ((tyv1,t1)::sub_compose tyv1 t1 sigma2))




let rec sub_substit ty a t = (*sub2_composeと全く同じだが分かりやすさのために新しく定義(aをtで置き換える)*)
  match ty with
  |TyInt -> TyInt 
  |TyBool ->  TyBool
  |TyFun(ty1,ty2)-> 
    TyFun(sub_substit ty1 a t,sub_substit ty2 a t)
  |TyVar tyv -> 
    if tyv = a then t else TyVar tyv
  |TyPair (ty1,ty2) -> 
    TyPair(sub_substit ty1 a t,sub_substit ty2 a t)
  |TyNil -> TyNil
  |TyCons ty -> 
    TyCons(sub_substit ty a t)

let rec substit cons a t =(*sub_composeと全く同じ機能を持つ.sub_substitを繰り返し呼ぶための関数*)
  match cons with 
  |[] -> []
  |(ty1,ty2)::rest -> 
    (sub_substit ty1 a t,sub_substit ty2 a t)::(substit rest a t)

let rec sub1_unify cons sigma= (*新しいσについてcomposeを用いてsigmaを更新する*)
  match cons with 
  |[] -> sigma
  |(TyFun(s,t),TyFun(s',t'))::c -> sub1_unify ((s,s')::(t,t')::c) sigma
  |(TyPair(s,t),TyPair(s',t'))::c -> sub1_unify ((s,s')::(t,t')::c) sigma
  |(TyCons t,TyCons t')::c -> sub1_unify ((t,t')::c) sigma
  |(s,t)::c -> 
    if s=t then sub1_unify c sigma else 
    (match (s,t) with
      |((TyVar a),t) -> (sub1_unify (substit c a t) (compose [(a,t)] sigma))
      |(t,(TyVar a)) -> (sub1_unify (substit c a t) (compose [(a,t)] sigma))
      |_ -> raise TyError)

let rec unify cons = 
  sub1_unify cons []




let rec look_up sub tyv = (*substからtyvをキーとしてσ(tyv)を返す関数*)
  match sub with
  |[] -> (TyVar tyv)
  |(k,v)::xs -> 
    if k = tyv then v else look_up xs tyv

let rec ty_subst sub t = (*型変数の代入σを用いてσ(t)の型を返す*)
  match t with 
  |TyInt -> TyInt
  |TyBool -> TyBool
  |TyFun(ty1,ty2) -> TyFun(ty_subst sub ty1,ty_subst sub ty2)
  |TyVar tyv -> 
    look_up sub tyv 
  |TyPair(ty1,ty2) -> TyPair(ty_subst sub ty1,ty_subst sub ty2)
  |TyNil -> TyNil
  |TyCons ty -> TyCons (ty_subst sub ty)



