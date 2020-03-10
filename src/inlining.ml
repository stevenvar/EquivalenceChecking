open Ast 

module FunMap = Map.Make(String)


let fresh_reset,fresh_name = 
  let cpt = ref 0 in 
  (fun () -> cpt := 0) , 
  (fun () -> incr cpt; "x_"^(string_of_int !cpt))

let create_map el =
  List.fold_left (fun map item -> 
      match item with 
      | Def (name,value) 
        | Recdef (name,value) -> FunMap.add name value map) FunMap.empty el


let print_map (m:expr FunMap.t) =
  FunMap.iter (fun name value ->
      Printf.printf "\nlet %s = " name; print_expr value) m

(* Change every recursive call of f to an uninterpreted function call  *)
let rec remove_rec_calls name f expr = 
  let r = remove_rec_calls name f in 
  match expr with 
  | Binop (op,e1,e2) -> Binop (op,r e1,r e2)
  | If (e1,e2,e3) -> If (r e1, r e2, r e3)
  | Let (s,e1,e2) -> Let (s,r e1,r e2)
  | Tuple el -> Tuple (List.map r el)
  | Seq (e1,e2) -> Seq (r e1, r e2)
  | Apply (e1,e) -> Apply (r e1, r e)
  | Variable g when f = g ->
     Uninterpreted name
  | Uninterpreted x ->
     Uninterpreted x
  | Lambda (sl,e) -> Lambda (sl, r e)
  | Variable s -> Variable s
  | Value i -> Value i

let remove_rec_calls_item f i =
  fresh_reset ();
  match i with 
  | Recdef (name,expr) when f = name -> Def(name,remove_rec_calls ("U_"^fresh_name ()) f expr)
  | _ -> i

let remove_rec_calls_ast f p = 
  List.map (remove_rec_calls_item f) p 

(* Inline function definition  *)
let inline f map item =
     let rec i expr =
       match expr with
       | Binop (op,e1,e2) -> Binop (op,i e1, i e2)
       | If (e1,e2,e3) -> If (i e1, i e2, i e3)
       | Let (s,e1,e2) -> Let (s,i e1,i e2) 
       | Tuple el -> Tuple (List.map i el)
       | Seq (e1,e2) -> Seq (i e1, i e2)
       | Apply (e1,e) -> Apply (i e1, i e)
       | Uninterpreted x -> Uninterpreted x
       | Variable g when f = g ->
          FunMap.find f map
       | Lambda (sl,e) -> Lambda (sl, i e)
       | Variable s -> Variable s
       | Value i -> Value i 
     in
    match item with 
    | Def (name,expr) -> Def (name,i expr)
    | Recdef (name,expr) -> Recdef (name,i expr)


let inline_ast f p =
  let m = create_map p in 
  List.map (inline f m) p 

(* Substitute s by e in e'  *)
let rec subst s e e' =
  match e' with
  | Value i -> Value i
  | Variable x when x = s -> e
  | Variable x -> Variable x
  | Uninterpreted x -> Uninterpreted x
  | Binop (op,e1,e2) -> Binop (op,subst s e e1, subst s e e2)
  | If (e1,e2,e3) -> If (subst s e e1, subst s e e2, subst s e e3)
  | Let (s',_,_) when s = s' -> failwith "name conflicts"
  | Let (s',e1,e2) -> Let (s',subst s e e1, subst s e e2) 
  | Tuple el -> Tuple (List.map (subst s e) el)
  | Seq (e1,e2) -> Seq (subst s e e1, subst s e e2)
  | Apply (e1,e2) -> Apply (subst s e e1, subst s e e2)
  | Lambda (Str x, _) when s = x -> failwith "name conflicts"
  | Lambda (x, e') -> Lambda (x,subst s e e')

(* Apply beta-reduction for function application *)
let rec reduce expr = 
  match expr with 
  | Binop (op,e1,e2) -> Binop (op,reduce e1, reduce e2)
  | If (e1,e2,e3) -> If (reduce e1, reduce e2, reduce e3)
  | Let (s,e1,e2) -> Let (s,reduce e1, reduce e2) 
  | Tuple el -> Tuple (List.map reduce el)
  | Seq (e1,e2) -> Seq (reduce e1, reduce e2)
  | Apply (Lambda (Str x,e),e') ->
     let e'' = subst x (reduce e') e in
     (* (a b c) = ((a b) c) so we reduce in the end *)
     reduce e''
  | Apply (e1,e) -> Apply (reduce e1, reduce e)
  | Lambda (s,e) -> Lambda (s,reduce e)
  | Variable s -> Variable s
  | Uninterpreted x -> Uninterpreted x
  | Value i -> Value i

(* Reducing until a fixpoint is found *)
let rec full_reduce exp = 
  let e = reduce exp in 
  if e = exp then exp 
  else 
    full_reduce e

let reduce_item i =
  match i with 
  | Def (name,expr) -> Def (name,full_reduce expr)
  | Recdef (name,expr) -> Recdef (name,full_reduce expr)

let reduce_ast = 
  List.map reduce_item

let rec simplify expr = 
  match expr with 
  | Binop (op,e1,e2) -> Binop (op,simplify e1, simplify e2)
  | If (e1,e2,e3) -> If (simplify e1, simplify e2, simplify e3)
  | Let (s,e1,e2) -> 
    let e2' = subst s (simplify e1) (simplify e2) in 
    simplify e2'
  | Tuple el -> Tuple (List.map simplify el)
  | Seq (e1,e2) -> Seq (simplify e1, simplify e2)
  | Apply (e1,e) -> Apply (simplify e1, simplify e)
  | Lambda (s,e) -> Lambda (s,simplify e)
  | Variable s -> Variable s
  | Uninterpreted x -> Uninterpreted x
  | Value i -> Value i

let simplify_item i =
  match i with 
  | Def (name,expr) -> Def (name, simplify expr)
  | Recdef (name,expr) -> Recdef (name,simplify expr)

let simplify_ast = 
  List.map simplify_item

 (* Alpha convert with new, fresh names *)
let rec rename_lambdas expr = 
  let rl = rename_lambdas in 
  match expr with 
  | Binop (op,e1,e2) -> Binop (op,rl e1, rl e2)
  | If (e1,e2,e3) -> If (rl e1, rl e2, rl e3)
  | Let (s,e1,e2) -> Let (s,rl e1,rl e2)   
  | Tuple el -> Tuple (List.map rl el)
  | Seq (e1,e2) -> Seq (rl e1, rl e2)
  | Apply (e1,e) -> Apply (rl e1, rl e)
  | Lambda (Str s,e) -> 
    let new_name = fresh_name () in 
    let e' = subst s (Variable new_name) e in
    Lambda (Str new_name,rl e')
  | Lambda (p,e) -> Lambda (p,rl e)
  | Variable s -> Variable s
  | Uninterpreted x -> Uninterpreted x
  | Value i -> Value i

let rename_lambdas_item i =
     fresh_reset ();
  match i with 
  | Def (name,expr) -> Def (name, rename_lambdas expr)
  | Recdef (name,expr) -> Recdef (name,rename_lambdas expr)

let rename_lambdas_ast =
    List.map rename_lambdas_item

let remove_function f p = 
  List.filter (function 
        Def (name,_) | Recdef (name,_) -> f <> name) p

let run f p =
  Printf.printf "\n\n------- PROGRAM ------- \n";
  (* Printf.printf "\nBEFORE :\n"; *)
  (* print_ast p; *)
  let k = remove_rec_calls_ast f p in 
  (* Printf.printf "\n\nAFTER REMOVING REC CALLS :\n"; *)
  (* print_ast k; *)
  let k = inline_ast f k in 
  Printf.printf "\n\nAFTER INLINING %s :\n" f;
  (* print_ast k; *)
  let k = reduce_ast k in
  (* Printf.printf "\n\nAFTER REDUCING :\n"; *)
  print_ast k;
  remove_function f k


let change f1 p1 f2 p2 = 
  let p1' = run f1 p1  in 
  let p2' = run f2 p2 in 
  (p1',p2')


let parsefile f = 
  let input = open_in f in 
  let filebuffer = Lexing.from_channel input in 
  let prog = Parser.main Lexer.token filebuffer in 
  close_in input;
  prog
