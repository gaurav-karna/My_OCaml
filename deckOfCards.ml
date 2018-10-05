(* Gaurav Karna // ID: 260723118) *)

exception Domain
exception NotImplemented

type suit = Clubs | Spades | Hearts | Diamonds

type rank =  Six | Seven | Eight | Nine | Ten |
             Jack | Queen | King | Ace

type card = rank * suit

type hand = Empty | Hand of card * hand

(* dom_suit : suit -> suit -> bool

   dom_suit s1 s2 = true iff suit s1 beats or is equal to suit s2
                    relative to the ordering S > H > D > C
   Invariants: none
   Effects: none
*)

let dom_suit s1 s2 = match s1, s2 with
  | Spades, _        -> true
  | Hearts, Diamonds -> true
  | Hearts, Clubs    -> true
  | Diamonds, Clubs  -> true
  | s1, s2           -> s1 = s2

type nat = int list (* increasing list of weights, each a power of two *)

(* --------------------------------------------------------------------*)
(* QUESTION 1: House of Cards                                          *)
(* --------------------------------------------------------------------*)

(* Q1: Comparing cards *)
(* Comparing two ranks *)
let dom_rank (r1 : rank) (r2 : rank) = 
  match (r1, r2) with
  | (Ace, _) -> true 
  | (King, _) -> if (r2 = Ace) then false else true
  | (Queen, _) -> if ((r2 = Ace) || (r2 = King)) then false else true
  | (Jack, _) -> if ((r2 = Ace) || (r2 = King) || (r2 = Queen)) then false else true
  | (Ten, _) -> if ((r2 = Ace) || (r2 = King) || (r2 = Queen) || (r2 = Jack)) then false else true
  | (Nine, _) -> if ((r2 = Nine) || (r2 = Eight) || (r2 = Seven) || (r2 = Six)) then true else false
  | (Eight, _) -> if ((r2 = Seven) || (r2 = Six) || (r2 = Eight)) then true else false
  | (Seven, _) -> if ((r2 = Six) || (r2 = Seven))then true else false
  | (Six, _) -> if (r2 = Six) then true else false 

(* Comparing two cards (r1, s1) and (r2, s2) *)
let dom_card (c1 : card) (c2 : card) =
  let (r1, s1) = c1
  in
  let (r2, s2) = c2
  in
  match ((r1, s1), (r2, s2)) with
  | (_, Spades), (_, Spades) -> dom_rank r1 r2
  | (_, Spades), (_,_) -> true
  | (_, Hearts), (_, Diamonds) -> true
  | (_, Hearts), (_, Clubs) -> true
  | (_, Hearts), (_, Hearts) -> dom_rank r1 r2
  | (_, Diamonds), (_, Clubs) -> true
  | (_, Diamonds), (_, Diamonds) -> dom_rank r1 r2 
  | (_, Clubs), (_, Clubs) -> dom_rank r1 r2
  | (_, _), (_, _) -> false
      


(* Q2: Insertion Sort – Sorting cards in a hand *)
let rec insert (c : card) (h : hand) : hand =
  match h with
  | Empty -> Hand(c, h)
  | Hand (c1, h1) -> if dom_card c c1 = true then Hand(c, h)
      else Hand(c1, insert c h1)
  
let rec sort (h : hand) : hand = 
  match h with
  | Empty -> h
  | Hand(c, h1) -> insert c (sort h1)
  (* if h = Empty then insert (Six, Clubs) h
   else 
     let Hand(c1, h2) = h 
     in
     match (c1, h2) with
     | ((Six, Spades), _) -> if dom_rank c1 (sort h2) = True then insert c1
         else insert (sort h2)
     | ((_, _), _) -> if dom_rank c1 (sort h2) = True then insert c1
         else insert (sort h2)
    
    let Hand(c1, h2) = 
      match h with
      | (Hand(r1, s1), Empty) -> insert (r1, s1) h
      | (Hand(r1, s1), Hand(r2, s2)) -> if dom_card (r1, s1) (r2, s2) = True
          then
            insert (r1, s1) (sort Hand(r2, s2))
          else
            insert (r2, s2) (sort Hand(r1, s1))
*)
(* Q3: Generating a deck of cards *)
let generate_deck (suits : suit list) (ranks : rank list) : card list = 
  (* let rec gen_one_card (onesuit) (rank) : card list =
     match (rank, onesuit) with
     | (_, _) -> (rank, onesuit)::[]
   in *)
  let rec gen_one_suit (onesuit : suit) (ranks' : rank list) : card list = 
    match ranks' with
    | [] -> []
    | r1::r2 -> (r1, onesuit)::(gen_one_suit onesuit r2)
  in 
  let rec gen_all_suits (suits' : suit list) (ranks2' : rank list) : card list =
    match suits' with
    | [] -> []
    | s::s2 -> (gen_one_suit s ranks2')@(gen_all_suits s2 ranks2')
  in
  gen_all_suits suits ranks
    (* match suits with 
      | Spades::s2 -> gen_one_suit Spades ranks *)
    (*| Spades::_ -> gen_one_suit Spades ranks
     | Clubs::_ -> gen_one_suit Clubs ranks
     | Hearts::_ -> gen_one_suit Hearts ranks
     | Diamonds::_ -> gen_one_suit Diamonds ranks *)

(* Q4: Shuffling a deck of cards *)
let rec split (deck : card list) (n : int) : card * card list = 
  match n with 
  | 0 -> (match deck with
      | [] -> ((King, Spades), []) (* should not be reached *)
      | (c1::tail) -> (c1, tail))
  | _ -> match deck with
    | [] -> ((King, Spades), []) (* should not be reached *)
    | (c1::tail) -> let (card, list) = 
                      split tail (n-1) 
        in
        (card, c1::list)
                    
     (* let rand_card = (List.nth deck n)
   in
   let new_list = []
   in
   let acc = 0
   in
   let rec split' (deck : card list) (n : int) : card list = 
     match deck with
     | [] -> []
     | (c1::tail) -> if (acc < n) then List.append (new_list@(c1::[])) (split' tail (acc+1))
         else List.append new_list tail
   in
   (rand_card, (split' deck n))
*)

let shuffle (deck : card list) : card list =
  let size = List.length deck in
  let rec select deck n =
    match n with
    | 0 -> []
    | _ -> let rand_index = Random.int n
        in
        let (c1, remain) = split deck rand_index
        in
        (c1::[])@(select remain (n-1))
  in
  select deck size
  

(* --------------------------------------------------------------------*)
(* QUESTION 2: Sparse Representation of Binary Numbers                 *)
(* ------------------------------------------------------------------- *)

(* Q1: Incrementing a sparse binary number *)
let inc (ws : nat) : nat = 
  let acc = 1
  in
  let rec get_total (acc : int) (ws : nat) : int =
    match ws with
    | [] -> acc
    | n1::tail -> get_total (acc+n1) tail
  in
  let totalToParse = get_total acc ws
  in
  (*let mod_list = []
   in *)
  (* let count = 2
   in *)
  let rec get_modulo (mod_list : int list) (total : int) (count : int) : int list =
    match total with
    | 0 -> mod_list
    | _ -> if (total mod 2 = 0) then get_modulo (mod_list) (total/2) (count*2)
        else 
          get_modulo (mod_list@((count)::[])) (total/2) (count*2)
  in
  get_modulo [] totalToParse 1
    (*match ws with
   | [] -> []
   | n1::tail -> *)
          
  (* (([2;]@tail) : nat) *)

(* Q2: Decrementing a sparse binary number *)
let dec (ws : nat) : nat =
  let acc = -1
  in
  let rec get_total (acc : int) (ws : nat) : int =
    match ws with
    | [] -> acc
    | n1::tail -> get_total (acc+n1) tail
  in
  let totalToParse = get_total acc ws
  in
  (*let mod_list = []
   in *)
  (* let count = 2
   in *)
  if (totalToParse < 0) then raise Domain else
    let rec get_modulo (mod_list : int list) (total : int) (count : int) : int list =
      match total with
      | 0 -> mod_list
      | _ -> if (total mod 2 = 0) then get_modulo (mod_list) (total/2) (count*2)
          else 
            get_modulo (mod_list@((count)::[])) (total/2) (count*2)
    in
    get_modulo [] totalToParse 1

(* Q3: Adding sparse binary numbers *)
let rec add (m : nat) (n : nat) : nat  =
  let acc = 0
  in
  let rec get_total (acc : int) (ws : nat) : int =
    match ws with
    | [] -> acc
    | n1::tail -> get_total (acc+n1) tail
  in
  let totalToParse = get_total acc m
  in
  (* now adding the leftover nat *)
  let grandTotal = get_total totalToParse n
  in
  let rec get_modulo (mod_list : int list) (total : int) (count : int) : int list =
    match total with
    | 0 -> mod_list
    | _ -> if (total mod 2 = 0) then get_modulo (mod_list) (total/2) (count*2)
        else 
          get_modulo (mod_list@((count)::[])) (total/2) (count*2)
  in
  (* using grand total of both nats *)
  get_modulo [] grandTotal 1
(* Q4: Converting to integer - tail recursively *)
let rec toInt (n : nat) (acc : int) : int =
  match n with
  | [] -> acc
  | (n1::tail) -> toInt tail (n1+acc)

let sbinToInt (n : nat) : int =
  let acc = 0 in
  match n with
  | [] -> acc
  | _ -> toInt n acc
