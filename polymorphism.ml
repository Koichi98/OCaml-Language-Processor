open TySyntax
open ConstraintSolver

let rec update_tyvls tyv tyv_ls rest= 
  match rest with 
  |[] -> tyv::tyv_ls
  |x::xs -> if x = tyv then tyv_ls
   else update_tyvls tyv tyv_ls xs 

let rec sub_get_type_vars ty tyv_ls =
    match ty with
    |TyInt-> tyv_ls
    |TyBool -> tyv_ls 
    |TyFun (ty1,ty2) -> 
      let newtyv_ls = sub_get_type_vars ty1 tyv_ls in
        (sub_get_type_vars ty2 newtyv_ls)@newtyv_ls
    |TyVar tyv -> 
      update_tyvls tyv tyv_ls tyv_ls
    |TyPair (ty1,ty2) ->
      let newtyv_ls = sub_get_type_vars ty1 tyv_ls in
        (sub_get_type_vars ty2 newtyv_ls)@newtyv_ls
    |TyNil -> tyv_ls
    |TyCons ty ->
      sub_get_type_vars ty tyv_ls


let get_type_vars ty = (*型に現れている型変数をすべてリストに列挙する*)
  sub_get_type_vars ty []


  
let rec instantiate_loop tyvls =
  match tyvls with 
  |[] -> [] 
  |tyv::rest -> 
    let alpha = new_tyvar () in
    (tyv,(TyVar alpha))::(instantiate_loop rest)

let instantiate scheme = (*変数に対して新しい型変数を代入*)
  let (tyvls,ty) = scheme in 
  ty_subst (instantiate_loop tyvls) ty
  




let rec sub3_generalize tyv tyv_ls =
  match tyv_ls with
  |[] -> false (*一致するものがなかったらfalse*)
  |tyv1::rest -> 
    if tyv = tyv1 then true else sub3_generalize tyv rest (*一致したらtrueを返す*)

let rec sub2_generalize tyv tyenv =
  match tyenv with
  |[] -> false
  |(n,(ls,t))::rest -> 
    let tyv_ls = get_type_vars t in 
    if sub3_generalize tyv tyv_ls then true (*trueがきたらtrueを返す*)
    else sub2_generalize tyv rest  (*falseが来たら型環境のリストの残りを確認する*)
  
let rec sub_generalize ls tyenv = 
  match ls with 
  |[] -> []
  |tyv::rest -> 
    if sub2_generalize tyv tyenv then sub_generalize rest tyenv (*trueがきたらtyvを削除してループ*)
    else tyv::(sub_generalize rest tyenv)(*falseが来たら型環境に含まれていないので,リストに追加*)

let rec generalize tyenv ty= (*一般化する関数:型環境にある型それぞれに含まれず、tyに含まれるものをリスト化し、tyとのペアにして返す*)
  let ls = get_type_vars ty in
  (sub_generalize ls tyenv,ty)
  (*(sub_generalize tyenv [],ty)*)



let rec del_bound_var_loop tyv tyv_ls =(*del_bound_varの補助関数*)
  match tyv_ls with 
  |[] -> false(*見つからなかったらfalse*)
  |tyv1::rest -> 
    if tyv = tyv1 then true (*sigmaの型変数に束縛変数があったらtrue*)
     else del_bound_var_loop tyv rest

let rec del_bound_var sigma tyv_ls =(*束縛変数(多相化されている変数)は更新しないためあらかじめsigmaから削除しておく関数*)
  match sigma with
  |[] -> []
  |(tyv,ty)::rest -> 
    if del_bound_var_loop tyv tyv_ls then del_bound_var rest tyv_ls(*見つかった場合はその型変数は更新しない*)
    else (tyv,ty)::(del_bound_var rest tyv_ls) 

let rec update_tyenv sigma tyenv = (*tyenvをsigmaで更新する関数*)
  match tyenv with 
  |[] -> []
  |(n,scheme)::rest ->
    let (ls,ty) = scheme in
    let deleted_sigma = del_bound_var sigma ls in
    let newty = ty_subst deleted_sigma ty in
    (n,(ls,newty))::(update_tyenv deleted_sigma rest) (*束縛変数のリストは更新しない?*)