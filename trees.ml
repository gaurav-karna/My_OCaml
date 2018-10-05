exception NotImplemented

(* -------------------------------------------------------------*)
(* QUESTION:  Tries                                             *)
(* -------------------------------------------------------------*)

(* A trie is an n-ary tree *)

type 'a trie = Node of 'a * ('a trie) list | Empty

(* -------------------------------------------------------------*)
(* Example trie list                                            *)
(* -------------------------------------------------------------*)

let t =
 [Node
     ('b',
      [Node ('r' , [Node ('e' , [Node ('e' , [Empty])])]) ;
       Node ('e' , [Empty ;
        Node ('e' , [Empty ; Node ('f', [Empty; Node ('y', [Empty])]) ;
         Node ('r',[Empty])]) ;
        Node ('a' , [Node ('r', [Empty; Node ('d' , [Empty])])])])])]

(* -------------------------------------------------------------*)
(* Implementation of tabulate                                   *)
(* tabulate f n returns [f 0; f 1; ...; f (n - 1)]              *)
(* -------------------------------------------------------------*)

let rec tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n - 1) ((f n) :: acc)
  in
  tab (n - 1) []

(* -------------------------------------------------------------*)
(* TRIE HELPER FUNCTIONS                                        *)
(* -------------------------------------------------------------*)

(* Creates a tree containing only l *)
(* unroll : 'a list -> 'a trie list *)
let rec unroll l = match l with
  | []     -> [Empty]
  | x :: t -> [Node(x, unroll t)]

(* Returns true if l contains an Empty node, false otherwise *)
(* contains_empty : 'a trie list -> bool *)
let rec contains_empty l = match l with
  | x :: xs -> x = Empty || contains_empty xs
  | []      -> false

(* 
(* Examples of functions that could be used for q5 predicates *)
type labeled_pred = string * (int -> bool)

let is_even : labeled_pred = ("Is even", fun n -> (n mod 2) = 0)
let mult_3 : labeled_pred = ("Is a multiple of 3", fun n -> (n mod 3) = 0)
let is_odd : labeled_pred = ("Is odd", fun n -> (n mod 2) <> 0)
let more_than_4 : labeled_pred = ("Is larger than 4", fun n -> n > 4)
let is_pow_of_2 : labeled_pred =
  let rec f = function
  | 0 -> false
  | 1 -> true
  | n -> if n mod 2 <> 0 then false else f (n / 2)
  in
  ("Is a power of 2", f) *)
(* -------------------------------------------------------------*)
(* QUESTION 1 : String manipulation  [20 points]                *)
(* -------------------------------------------------------------*)

(* string_explode : string -> char list *)
let string_explode s = 
  let explode' n =
    s.[n]
  in
  let len = String.length s
  in
  tabulate explode' len
  (* my implementation w/o HOF
    let toReturn = []
    in
    let cnt = 0
    in
    let len = String.length s
    in
    let rec explode' s cnt toReturn = 
      if cnt = len then
        toReturn
      else 
        explode' s (cnt+1) (toReturn@s.[cnt]::[])
    in
    explode' s cnt toReturn *)

  
(* string_implode : char list -> string *)
let string_implode l =
  let current = ""
  in
  let implode' theList current =
    let toJoin = Char.escaped theList
    in
    toJoin ^ current
  in
  (* passing in a char list and string *)
  List.fold_right implode' l current

(* -------------------------------------------------------------*)
(* QUESTION 2 : Insert a string into a dictionary  [20 points]  *)
(* -------------------------------------------------------------*)

(* Insert a word into a dictionary. Duplicate inserts are allowed *)

(* IGNORE ALL CODE BELOW UNTIL LINE 93 *)
    (* let insert s t =
       let before = []
       in
  (* ins: char list * char trie list -> char trie list *) 
       let rec ins l t' before =
         match l with
         | [] -> t@(unroll l)
              (* h::tail is char::rest of char list *)
         | h::charTail -> (match t' with
             | [] -> unroll l
             | nodeCheck::nodeTail -> (match nodeCheck with
                 | Empty -> before@(nodeCheck::[])@(unroll l)@nodeTail
                 | Node (nodeChar, underNode) -> if nodeChar = h then
                       (match underNode with
                        | [Empty] -> (match charTail with 
                            | [] -> before@(Node(nodeChar, [Empty])::[])@nodeTail
                           (* before@(nodeCheck::[])@[Empty]@nodeTail *) (* instead of nodeCheck::[Empty] since this
                                    would be duplicate key *)
                            | _ -> before@(Node(nodeChar, (unroll l))::[])@nodeTail )
                                                                                    (* before@(Node(nodeChar, (unroll l))::[])@nodeTail *)(* t@(unroll l) ) *)
                     (*l@((Node (nodeChar, (unroll charTail)) )::[]) *)
                        | _ -> (match charTail with
                            | [] -> before@(Node(nodeChar, [Empty])::[])@nodeTail
                               (* before@(Node(nodeChar, [Empty])::[])@nodeTail *)
                                                                                  (* nodeCheck::[Empty] *)
                            | _ ->  ins charTail underNode (before@(nodeCheck::[])) ))
                     else
                       t@(unroll l) ))
                  (* if (h::charTail = (string_explode s)) then
                      match nodeTail with
                      | [] -> t@(unroll l)
                      | _ -> t@(ins l nodeTail)
                    else
                      t@(unroll l))) *)
                  (* if (h::charTail = (string_explode s)) then
                      t@(unroll l) 
                    else 
                  (* ins l nodeTail *)
                      t@(unroll l) )) *)
                  (*ins l nodeTail *)
                    (* let t = [Node(x, y)] in
                  match h with
                  | Node(x, y) -> if h = x then ins tail y
                      else unroll l *)
  in
  ins (string_explode s) t before *)
    
let insert s t =
  let rec ins cl t =
    match cl with
    | [] -> Empty :: t
    | c::cl' -> match t with
      | [] -> unroll cl
      | Empty :: t' -> Empty :: (ins cl t')
      | Node(c', r) :: t' -> 
          if c = c' 
          then Node(c', ins cl' r) :: t'
          else Node(c', r) :: (ins cl t')
  in 
  ins (string_explode s) t
    (*
      match (cl, t) with
      | [], _ -> Empty :: t
      | _, Empty :: t' -> Empty :: (ins cl t')
*)

(* -------------------------------------------------------------*)
(* QUESTION 3 : Look up a string in a dictionary   [20 points]  *)
(* -------------------------------------------------------------*)

(* Look up a word in a dictionary *)

let lookup (s : string) t =
  (* lkp : char list -> char trie list -> bool *) 
  let rec lkp l t' = 
    match l with
      (* | [] -> if t' = [Empty] then true else false *)
    | [] -> (match t' with
        | [Empty] -> true
        | Empty::restt' -> true
        | [] -> false
        | first::sub -> if contains_empty t' = true then true else false)
    | toMatch::rest -> match t' with
      | [] -> false
      | theTrie::restTrie -> match theTrie with
        | Empty -> lkp l restTrie
        | Node (theChar, subTrie) -> if theChar = toMatch then
              (* if contains_empty subTrie = true then *)
              lkp rest subTrie
            else lkp l restTrie
                (*else
if contains_empty restTrie = true then
  lkp rest restTrie 
else false *)
  in
  lkp (string_explode s) t
(* -------------------------------------------------------------*)
(* QUESTION 4 : Find all strings in a dictionary   [OPTIONAL]   *)
(* -------------------------------------------------------------*)

(* Find all strings which share the prefix p *)

let find_all prefix t =
  (* find_all' : char list -> char trie list -> char list list *)
  let rec find_all' l t =
    raise NotImplemented
  in
  raise NotImplemented

(* -------------------------------------------------------------*)
(* QUESTION 5 :  Logic functions   [OPTIONAL]                   *)
(* -------------------------------------------------------------*)

(* eval: labeled_pred -> labeled_pred -> int -> int -> bool *)
let eval (_, (p : int -> bool)) (_, (q : int -> bool)) (n : int) =
  raise NotImplemented
