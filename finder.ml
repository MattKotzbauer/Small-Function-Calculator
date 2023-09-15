
let append list1 list2 = 
  List.append list1 list2

type term =
  | Const of float
  | X
  | Plus of term * term
  | Minus of term * term
  | Times of term * term
  | Divide of term * term
  | Hole 

let rec has_hole term =
  match term with
  | Hole -> true
  | Const _ | X -> false
  | Plus (t1, t2) | Minus (t1, t2) | Times (t1, t2) | Divide (t1, t2) ->
      has_hole t1 || has_hole t2

let rec evaluate term x =
  match term with
  | Const c -> c
  | X -> x
  | Plus (t1, t2) -> evaluate t1 x +. evaluate t2 x
  | Minus (t1, t2) -> evaluate t1 x -. evaluate t2 x
  | Times (t1, t2) -> evaluate t1 x *. evaluate t2 x
  | Divide (t1, t2) -> evaluate t1 x /. evaluate t2 x
  | Hole -> failwith "Cannot evaluate Hole"

let rec generate_expansions expandable_term n = 
  match n with
  | 0 -> expandable_term
  | _ -> 
    match expandable_term with
    | Hole -> Const 5.0 (* (this is the result we eventually want to vary )*)
    | Const _ | X -> expandable_term
    | Plus(t1, t2) ->
    if has_hole t1 then Plus(generate_expansions t1 (n-1), t2)
    else if has_hole t2 then Plus(t1, generate_expansions t2 (n-1))
    else Plus(t1, t2)

let rec string_of_term term =
  match term with
  | Const c -> string_of_float c
  | X -> "X"
  | Plus (t1, t2) -> "(" ^ string_of_term t1 ^ " + " ^ string_of_term t2 ^ ")"
  | Minus (t1, t2) -> "(" ^ string_of_term t1 ^ " - " ^ string_of_term t2 ^ ")"
  | Times (t1, t2) -> "(" ^ string_of_term t1 ^ " * " ^ string_of_term t2 ^ ")"
  | Divide (t1, t2) -> "(" ^ string_of_term t1 ^ " / " ^ string_of_term t2 ^ ")"
  | Hole -> "Hole"

let () = 
  let testable_fns = [| Const 2.0; X |] in
  let expandable_fns = [| Plus(Hole, Hole) |] in
  let expanded_term = generate_expansions expandable_fns.(0) expandable_fns in
  print_endline (string_of_term expanded_term); 
  (* Printf.printf "Expanded Term: %s\n" (string_of_term expanded_term) *)



