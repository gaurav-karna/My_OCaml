exception NotImplemented

(* Question 1 : Let's have cake! *)

type price = float
type weight = float
type calories = int
type ingredient = Nuts | Gluten | Soy | Dairy

type cupcake = Cupcake of price * weight * calories * ingredient list

let c1 = Cupcake (2.5, 80.3, 250, [Dairy; Nuts])
let c2 = Cupcake (2.75, 90.5, 275, [Dairy; Soy])
let c3 = Cupcake (3.05, 100.4, 303, [Dairy; Gluten; Nuts])
let c4 = Cupcake (3.25, 120.4, 330, [Dairy; Gluten ])

let cupcakes = [c1 ; c2 ; c3 ; c4]

(* Question 2 : Generic Tree Traversals *)

type 'a tree = Node of 'a * 'a tree * 'a tree | Empty
(* -------------------------------------------------------------*)
(* QUESTION 1 : Let's have cake!                                *)
(* -------------------------------------------------------------*)

(* allergy_free : ingredient list -> cupcake list -> cupcake list *)
let allergy_free allergens cupcakes = 
  let rec goodCakes' allergen ingredients =
    match allergen with
    | [] -> true
    | h::tail -> match ingredients with
      | [] -> goodCakes' tail ingredients
      | _ ->  if List.exists (fun first -> h = first) ingredients = true
          then
            false
          else
            goodCakes' tail ingredients
  in
  if (List.for_all (fun cupcake ->
      match cupcake with
      | Cupcake (_, _, _, []) -> true
      | Cupcake (_, _, _, ingredients) -> (goodCakes' allergens ingredients)) cupcakes) = true
  then cupcakes else 
    List.filter (fun cupcake ->
        match cupcake with
        | Cupcake (_, _, _, []) -> true
        | Cupcake (_, _, _, ingredients) -> (goodCakes' allergens ingredients)) cupcakes 
  
(* -------------------------------------------------------------*)
(* QUESTION 2 : Generic Tree Traversals                         *)
(* -------------------------------------------------------------*)

(* map_tree : ('a -> 'b) -> 'a tree -> 'b tree *)
let rec map_tree f t =
  match t with
  | Empty -> Empty 
  | Node (a, b1, b2) -> Node (f a, map_tree f b1, map_tree f b2)

(* delete_data : ('a * 'b) tree -> 'a tree *)
let delete_data t = map_tree (fun (a, b) -> a) t
  (* let delete' t' = 
     match t' with
     | Empty -> Empty
     | Node ((a, b), b1, b2) -> Node (a, b1, b2) 
                                  (* match a with
        | (Empty, _) -> Empty
        | (key, value) -> Node (key, b1, b2) *)
                               (* Node (a, (delete' b1), (delete' b2)) *)
                                  (* match (a, b) with
        | (Empty, Empty) -> Empty
        | Node ((key, value), b1, b2) -> Node (key, b1, b2) *)
   in 
   match t with
   | Empty -> Empty
   | Node (a, b1, b2) -> map_tree delete' t *)

(* fold_tree : ('a * 'b ' * 'b -> 'b) -> 'b -> 'a tree -> 'b *)
let rec fold_tree f e t =
  match t with
  | Empty -> e
  | Node (a, b1, b2) -> f (a, (fold_tree f e b1), (fold_tree f e b2)) 
                                                                         (* match b1 with
      | Empty -> e
      | Node (a', b1', b2') -> match b2 with
        | Empty -> e
        | Node (a'', b1'', b2'') -> f a a' a'' *)

(* size : 'a tree -> int *)
let size tr = fold_tree (fun (a, l_res, r_res) -> l_res + r_res + 1) 0 tr

(* reflect : 'a tree -> 'a tree *)
let reflect tr = fold_tree (fun (a, l_res, r_res) -> Node (a, r_res, l_res)) Empty tr
(* inorder : 'a tree -> 'a list *)
let inorder tr = fold_tree (fun (a, l_res, r_res) -> l_res@(a::r_res)) [] tr
    
    



