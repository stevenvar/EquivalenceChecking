

let compare_maps m1 m2 =
  let open Inlining in
  let must_keep f v =
    not (FunMap.mem f m2) || FunMap.find f m2 <> v
  in
  FunMap.filter (fun k v -> must_keep k v) m1 |> FunMap.bindings |> List.map fst


let get_roots m =
  let is_function =
    function Ast.Lambda (_,_) -> true
           | _ -> false
  in
  Inlining.FunMap.filter (fun _ v -> not (is_function v)) m

  
let rec call_graph e =
  let (++) = List.append in 
  let open Ast in 
  match e with
  | Value _ -> []
  | Variable s -> [s]
  | Tuple (es) -> List.fold_left (fun acc x -> (call_graph x) ++ acc) [] es
  | Binop (_,e1,e2) ->
     call_graph e1 ++ call_graph e2
  | If (e1,e2,e3) ->
     call_graph e1 ++ call_graph e2 ++ call_graph e3
  | Seq (e1,e2) ->
     call_graph e1 ++ call_graph e2
  | Let (x,e1,e2) ->
     call_graph e1 ++ List.filter (fun y -> y <> x) (call_graph e2)
  | Apply (e1,e2) ->
     call_graph e1 ++ call_graph e2
  | Lambda (Str x,e) ->
     List.filter (fun y -> y <> x) (call_graph e)
  | Lambda (_,e) ->
     (call_graph e)
  | _ -> []

let compare_graph v1 v2 =
  let deps1 = call_graph v1 in
  let deps2 = call_graph v2 in
  v1 = v2 && (List.fold_left2 (fun acc x1 x2 -> x1 = x2 && acc) true deps1 deps2)  
    
let _ =
  let open Inlining in
  (* Get the files *)
  let f1 = Sys.argv.(1) in
  let f2 = Sys.argv.(2) in
  let p1 = parsefile f1 in
  let p2 = parsefile f2 in
  (* Remove let in to prevent name capture  *)
  let p1 = simplify_ast p1 in
  let p2 = simplify_ast p2 in
  (* Alpha-convert everything  *)
  let p1 = rename_lambdas_ast p1 in
  let p2 = rename_lambdas_ast p2 in
  Ast.print_ast p1;
  Printf.printf "\n---\n";
  Ast.print_ast p2;
  (* Get the maps (function,value) for both programs *)
  let m1 = create_map p1 in
  let m2 = create_map p2 in
  let l = compare_maps m1 m2 in
  (* Apply every inlining *)
  let p1' = List.fold_left (fun acc f ->
                run f acc) p1 l in
  Printf.printf "\n\n*******\n";
  (* The same with maps reversed *)
  let l = compare_maps m2 m1 in
  (* Apply every inlining *)
  let p2'  = List.fold_left (fun acc f ->
                 run f acc) p2 l in
  (* Check equality (might bug if the result is empty lists TO FIX)) *)
  Printf.printf "\n\nResult:\n";
  Ast.print_ast p1';
  Printf.printf "\n---\n";
  Ast.print_ast p2';
  Printf.printf "\n---\n";

  let roots1 = get_roots (create_map p1') in
  let roots2 = get_roots (create_map p2') in
  FunMap.iter (fun k v -> Printf.printf "\nRoots : let %s ="
                            k;
                          Ast.print_expr v) roots1;
  FunMap.iter (fun k v -> Printf.printf "\nRoots : let %s ="
                            k;
                          Ast.print_expr v) roots2;
  let result = FunMap.fold (fun k v acc ->
      compare_graph v (FunMap.find k roots2) && acc) roots1 true in
  let result = FunMap.fold (fun k v acc ->
      compare_graph v (FunMap.find k roots1) && acc) roots2 result in
  
  Printf.printf "\nAre the progs equals? %b\n" result
  
    
    
  (* if p1' = [] && p2' = [] then failwith "empty programs";
   * Printf.printf "\nAre the programs equivalent ? %b\n" (p1'=p2') *)
  
  
