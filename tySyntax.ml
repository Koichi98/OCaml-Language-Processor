type tyvar = Alpha of int 
	      
type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar


let count = ref 0

let new_tyvar () = 
  let c = !count in count := c+1;Alpha (c+1)

let rec print_type t = 
  match t with 
  |TyInt -> Printf.printf " int"
  |TyBool -> Printf.printf " bool"
  |TyFun(ty1,ty2) -> print_type ty1;Printf.printf " ->";print_type ty2
  |TyVar tyv -> let Alpha i = tyv in Printf.printf " 'a";print_int i

let rec print_type_list ls= (*デバッグ用,生成された型制約の出力*)
  match ls with
  | [] -> Printf.printf "[]"
  |(a,b)::xs -> Printf.printf "(";print_type a;Printf.printf ","; print_type b;Printf.printf ")";Printf.printf ";"; print_type_list xs 
