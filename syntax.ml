type name = string 

type pattern =
  | PInt  of int
  | PBool of bool
  | PVar  of name
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern

type 'a option = Some of 'a list | None

type expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name 
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EAnd       of expr * expr
  | EOr        of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr		 
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ENot       of expr
  | EFun       of name * expr
  | EApp       of expr * expr
  | ELetRec    of (name * name * expr) list * expr
  | EPair      of expr * expr
  | ENil
  | ECons      of expr * expr
  | EMatch     of expr * (pattern * expr) list

type value =
  | VInt  of int
  | VBool of bool 
  | VErr of string      (*エラー処理用のvalue型*)
  | VFun  of name* expr * env ref
  | VPair of value * value
  | VNil
  | VCons of value * value
  and 
   env = (name * thunk) list
  and 
   thunk = Thunk of expr * env ref

type command =
  | CExp of expr
  | CDecl of name * expr
  | PLErr
  | CRecDecl of (name * name * expr) list 
				  
let print_name = print_string 


let rec print_value v =
  match v with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun (n,e,env) -> Printf.printf " <fun>" 
  | VPair(v1,v2) -> Printf.printf "(";print_value v1;Printf.printf "," ;print_value v2;Printf.printf ")" 
  | VNil -> Printf.printf "[]";
  | VCons(v1,v2) -> Printf.printf "[";print_value v1;print_list v2
  and 
print_list v = 
   match v with
   | VNil -> Printf.printf "]";
   | VCons(v1,v2) -> Printf.printf "; ";print_value v1;print_list v2 



(*let rec print_expr e =
  match e with
  | EConstInt i ->
     print_int i
  | EConstBool b ->
     print_string (string_of_bool b)
  | EVar x -> 
     print_name x
  | EAdd (e1,e2) -> 
     (print_string "EAdd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ESub (e1,e2) -> 
     (print_string "ESub (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EMul (e1,e2) -> 
     (print_string "EMul (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EDiv (e1,e2) -> 
     (print_string "EDiv (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EEq (e1,e2) ->
     (print_string "EEq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ELt (e1, e2) ->
     (print_string "ELt (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EIf (e1,e2,e3) ->
     (print_string "EIf (";
      print_expr   e1;
      print_string ","; 
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")")

let rec print_command p =       
  match p with
  | CExp e -> print_expr e*)
