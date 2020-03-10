

let compare_maps m1 m2 =
  let open Inlining in
  (* TO FIX *)
  (* The issue with that if all the functions depending on a rename are themselves
   * different (since they don't call the same function)
   * a solution would be to do the already computed inlines before comparing further functions *)
  (* The other issue is that since we remove the functions that we inline, we can end up with two empty programs for two completely different programs, which would say that they are equal *)
  let must_keep f v =
    not (FunMap.mem f m2) || FunMap.find f m2 <> v
  in
  FunMap.filter (fun k v -> must_keep k v) m1 |> FunMap.bindings |> List.map fst
    
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
  if p1' = [] && p2' = [] then failwith "empty programs";
  Printf.printf "\nAre the programs equivalent ? %b\n" (p1'=p2')
  
  
