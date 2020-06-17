open Syntax
open Eval
open Infer
       
let rec read_eval_print env tyenv=
  print_string "# ";
  flush stdout;
  let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  let (t,newtyenv) = infer_cmd tyenv cmd in
  let (id, newenv, v) = eval_command env cmd in
    if (id = "Error") then (print_newline ();read_eval_print newenv newtyenv) (*idが"Error"の場合には改行して次の入力を待つ*)
    else
    print_newline ();
    (Printf.printf "%s :" id;
    TySyntax.print_type t;
    print_value v;
    print_newline ();
    read_eval_print newenv newtyenv)

let initial_env =
  extend "i" (VInt 1)
	 (extend "v" (VInt 5)
		 (extend "x" (VInt 10)
			 empty_env))
    
let _ = read_eval_print initial_env []
