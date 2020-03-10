type expr = Value of int
         | Variable of string
         | Tuple of expr list
         | Binop of op * expr * expr
         | If of expr * expr * expr
         | Seq of expr * expr
         | Let of string * expr * expr
         | Apply of expr * expr
         | Lambda of pat * expr
         | Uninterpreted of string 

and pat =
  | Unit
  | Str of string

and op = 
  | Sub 
  | Mul 
  | Add
  | Eq

type item = 
  | Def of string * expr 
  | Recdef of string * expr

type ast = item list

let f = Printf.printf

let print_pat  =
  function
  | Unit -> f "()"
  | Str s -> f "%s" s

let print_op = 
  function 
  | Mul -> f "*"
  | Add -> f "+"
  | Sub -> f "-"
  | Eq -> f "=="

let rec print_expr =
  function
  | Value i -> f "%d" i
  | Variable s -> f "%s" s
  | Uninterpreted s -> f "%s*" s
  | Tuple el -> f "("; List.iter (fun x -> print_expr x; f "\n") el; f ")"
  | Binop (op,e1,e2) -> f "("; print_expr e1 ; print_op op; print_expr e2 ; f ")"
  | If (e1,e2,e3) -> f "(if "; print_expr e1; f " then "; print_expr e2; f " else "; print_expr e3; f ")"
  | Seq (e1,e2) -> print_expr e1 ; f ";"; print_expr e2
  | Let (s,e1,e2) -> f "let %s = " s; print_expr e1 ; f " in "; print_expr e2
  | Apply (e, e') ->
     f "(";
     print_expr e;
     f " ";
     print_expr e';
     f ")"
  | Lambda (arg,e) ->
     f "(fun ";
     print_pat arg;
     f " -> ";
     print_expr e;
     f ")"


let print_item i =
  match i with 
  | Def (name,expr) -> Printf.printf "\nlet %s = " name ; print_expr expr
  | Recdef (name,expr) -> Printf.printf "\nlet rec %s = " name;  print_expr expr  

let print_ast = 
  List.iter print_item 

