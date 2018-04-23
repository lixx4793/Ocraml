(* Length:  a' list -> int *)
let length ls =
let inc x y =x+1 in
	List.fold_left inc 0 ls
	

(* Rev ls: 'a list -> a' list *)
let rev ls =
List.fold_left (fun x y -> y::x) [] ls




(* is_elem_by: ('a ->'b-> bool) -> 'b ->'a list ->bool *)
let is_elem_by f num ls =
let fliterls = List.filter (fun x ->f x num) ls in 
match fliterls with
| [] -> false
| x::rest -> true



(*is_elem: ('a ->'a list -> bool *)
let is_elem num ls = 
if is_elem_by (=) num ls then true else false


(* Removing duplicates from a list: 'a list -> 'a list *)
let dedup ls =
List.fold_right (fun num l -> if is_elem num l then l else num :: l) ls []



(* split_by*)
let split_by f ele ls = 
let combain (a,b) = a::b in
combain
(List.fold_right (fun x (a,b) ->if is_elem_by f x ls then ([],a::b) else 
(x::a,b)) ele ([],[]))



type word = char list
type line = word list


let changeType option =
match option with 
Some x -> x 
|_->[]

let spLine ls =
split_by (=) ls ['\n']

let convert_to_non_blank_lines_of_words  option = 
let ls = spLine option in
let del m = m!=[] in
List.fold_right (fun x y ->
(List.filter del (split_by (=) x [' ';',';'.';'!';'?';':';'-']))::y)
ls []






(* This file contains a few helper functions and type declarations
   that are to be used in Homework 2. *)

let rec take n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then x::take (n-1) xs else []

let rec drop n l = match l with
  | [] -> [] 
  | x::xs -> if n > 0 then drop (n-1) xs else l




(* Some functions for reading files. *)
let read_file (filename:string) : char list option =
  let rec read_chars channel sofar =
    try 
      let ch = input_char channel
      in read_chars channel (ch :: sofar)
    with
    | _ -> sofar
  in
  try 
    let channel = open_in filename
    in 
    let chars_in_reverse = read_chars channel []
    in Some (rev chars_in_reverse)
  with
    _ -> None

let tols file = 
let option = read_file file in
match option with 
Some x ->List.map (fun x -> Char.uppercase x) x
|_->[]

type result = OK
	|FileNotFound of string
	|IncorrectNumLines of int
	|IncorrectLines of (int*int) list
	|IncorrectLastStanza





let helper ls = let ele1 =if (take 1 ls = take 1 (drop 1 ls)) then [] else [(1,2)] in
let ele2 = if (take 1 (drop 2 ls) = take 1 (drop 3 ls)) then [] else [(3,4)] in
let ele3 = if (take 1 (drop 6 ls) = take 1 (drop 7 ls)) then []  else [(7,8)] in
let ele4 = if (take 1 (drop 8 ls) = take 1 (drop 8 ls)) then [] else [(9,10)] in
let ele5 = if (take 1 (drop 12 ls) = take 1 (drop 12 ls)) then []  else [(13,14)] in
let ele6 = if (take 1 (drop 14 ls) = take 1 (drop 15 ls)) then [] else [(15,16)] in




let ele56 = if List.sort compare(dedup(List.concat(List.concat((take 1 ls) ::(take 1 (drop 1 ls)):: (take 1 (drop 2 ls))::[])))) =
List.sort compare (dedup(List.concat(List.concat((take 1 (drop 4 ls)) :: (take 1 (drop 5 ls)) :: [] )))) then 
[] else [(5,6)] in




let ele1112 = if List .sort compare((List.concat(List.concat(take 1 (drop 6 ls) ::(take 1 (drop 9 ls)) ::[] )))) =
List.sort compare ( (List.concat(List.concat(take 1 (drop 10 ls) :: (take 1 (drop 11 ls)) ::[])))) then
[] else [(11,12)] in
let ele1718 = if List.sort compare((List.concat(List.concat(take 1 
(drop 13 ls) :: (take 1 (drop 15 ls)) ::[])))) =
List.sort compare (( List.concat(List.concat(take 1 (drop 16 ls):: 
(take 1 (drop 17 ls)) ::[])))) then
[] else [(17,18)] in
ele1::ele2::ele56::ele3::ele4::ele1112::ele5::ele6::ele1718::[]

let checksa ls =
let front = List.sort compare (dedup(List.concat(List.concat( (take 1 ls) 
:: (take 1 (drop 2 ls)) :: (take 1 (drop 6 ls)) ::(take 1 (drop 8 ls)) 
:: (take 1 (drop 12 ls)) :: (take 1 (drop 14 ls)) :: [])))) in
let back = List.sort compare (dedup(List.concat(List.concat( (take 1 
(drop 18 ls)) :: (take 1 (drop 19 ls)) :: (take 1 (drop 20 ls)) ::
 (take 1 (drop 21 ls )) :: (take 1 (drop 22 ls)) :: (take 1 (drop 23 ls))::[])))) in
front = back

let paradelle file = 
match (read_file file) with
|None -> FileNotFound file
|Some x-> 
let prels = convert_to_non_blank_lines_of_words(tols file) in
let ls = List.filter (fun x -> x !=[]) prels in
let l = length(ls) in
if l!=24 then IncorrectNumLines l 
else if ((List.concat(helper ls) = [])) =false then IncorrectLines (List.concat (helper ls))
else if (checksa ls = false ) then IncorrectLastStanza
else OK
