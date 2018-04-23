
type formula = And of formula * formula
	     | Or  of formula * formula
	     | Not of formula 
	     | Prop of string
	     | True
	     | False

type subst = (string * bool) list

let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

let show_string_bool_pair (s,b) =
  "(\"" ^ s ^ "\"," ^ (if b then "true" else "false") ^ ")"

let show_subst = show_list show_string_bool_pair


let is_elem v l =
  List.fold_right (fun x in_rest -> if x = v then true else in_rest) l false

let rec explode = function
  | "" -> []
  | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

let dedup lst =
  let f elem to_keep =
    if is_elem elem to_keep then to_keep else elem::to_keep
  in List.fold_right f lst []

let rec lookup (n:string) (env:subst) : bool=
  match env with
  | [] -> false
  | (x,value)::rest when x = n -> value
  | _::rest -> lookup n rest

exception KeepLooking
exception StopLooking
	(************************************eval*************************************)
let rec eval formula env =
match formula with 
|And(form1, form2) ->( eval form1 env) && (eval form2 env)
|Or(form1, form2) -> (eval form1 env ) || (eval form2 env)
|Not form -> if (eval form env) then false else true
|Prop str -> lookup str env
|True -> true
|False -> false
	
	(************************************freevars********************************)
let freevars formula = 
let rec helper formula env =
match formula with
|And(form1, form2) -> let ls = helper form1 env in ls @ (helper form2  (ls@env))
|Or(form1, form2) -> let ls = helper form1 env in ls @ (helper form2  (ls@env))
|Not form1 -> helper form1 env
|Prop str -> if (List.filter (fun x-> x =str) env) = [] then [str] else []
|True -> []
|False -> []
in helper formula []


(****************************************is_tau******************************)




 let is_tautology formula f = 
let rec try_subset environment rest =
   if (eval formula environment=false) && environment <> [] && rest = []
   then f  environment 
    else match rest with
	 | [] -> raise KeepLooking
	 | x::xs -> try try_subset (environment @ [(x, true)]) xs with
		    | KeepLooking -> try_subset (environment @[(x,false)])  xs

  in try try_subset [] (freevars formula) with
     | KeepLooking -> None






let is_tautology_first f = is_tautology f (fun s -> Some s)

  let is_tautology_print_all f =
  is_tautology 
    f
    (fun s -> print_endline (show_subst s); 
	      raise KeepLooking)
  

	      


(****************************************Maze*****************************)
let rec is_not_elem set v =
  match set with
  | [] -> true
  | s::ss -> if s = v then false else is_not_elem ss v

let is_elem v l =
  List.fold_right (fun x in_rest -> if x = v then true else in_rest) l false

let ok_movement position destination=
match position with 
|(1,1) -> if destination = (1,2) then false else true
|(2,1) -> if destination = (2,2) then false else true
|(3,1) -> if destination = (4,1) then false else true
|(4,1) -> if (is_elem destination [(3,1);(5,1)]) then false else true
|(5,1) -> if destination = (4,1) then false else true
|(1,2) -> if destination = (1,1) then false else true
|(2,2) -> if (is_elem destination [(2,1);(2,3)]) then false else true
|(3,2) -> true
|(4,2) -> if (is_elem destination [(4,3);(5,2)]) then false else true
|(5,2) -> if destination = (4,2) then false else true
|(1,3) -> true
|(2,3) -> if (is_elem destination [(2,2);(3,3);(2,4)]) then false else true 
|(3,3) -> if destination = (2,3) then false else true
|(4,3) -> if (is_elem destination [(4,4);(4,3)]) then false else true
|(5,3) -> true
|(1,4) -> if destination = (2,4) then false else true
|(2,4) -> if (is_elem destination [(2,3);(1,4)]) then false else true
|(3,4) -> if destination = (3,5) then false else true
|(4,4) -> if (is_elem destination [(4,3);(5,4)]) then false else true
|(5,4) -> if (is_elem destination [(4,4);(5,5)]) then false else true
|(1,5) -> true
|(2,5) -> if (destination = (3,5)) then false else true
|(3,5) -> true
|(4,5) ->true
|(5,5) -> if destination = (5,4) then false else true




let maze_move position = 
let move_up (y,x) = if  (y-1)>0 then [(y-1, x)] else [] in
let move_down(y,x) =if  (y+1)<6 then [(y+1,x)] else [] in 
let move_right(y,x)=if  (x+1)<6 then[(y,x+1)] else [] in
let move_left(y,x) =if  (x-1)>0 then [(y,x-1)] else []in
List.filter (ok_movement position) (move_up position @ move_down position@ move_right position @ move_left position)

let final =[(5,1);(3,5)]



let maze() = 
	let rec go_from current path =
		if (is_elem current final) then Some path
		else 
		match List.filter (is_not_elem path) (maze_move current) with
		|[] -> None
		|[(a,b)] -> (go_from (a,b) (path @ [(a,b)]))
		|[(a,b);(c,d)]->
			(match (go_from (a,b) (path @ [(a,b)])) with
			|Some path' -> Some path'
			|None -> go_from (c,d) (path @ [(c,d)])
			)
		|[(a,b);(c,d);(e,f)]->
			(match go_from (a,b) (path @ [(a,b)]) with 
			|Some path1 -> Some path1
			|None ->
				(match go_from (b,c) (path @ [(b,c)]) with 
				|Some path1 -> Some path1
				|None -> go_from (e,f) (path @ [(e,f)])
				)
			)	
		|_ -> raise (Failure ("Not possible to have 4 direction to go"))
		in go_from (2,3) [(2,3)]
		
			
		
		
		
		
		
		
		 
		
		
		
		
		
		
		
		
		
		
		
		
		
