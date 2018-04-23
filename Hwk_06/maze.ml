(****************************************Maze*****************************)
exception KeepLooking
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
		|[] -> raise KeepLooking
		|[(a,b)] -> (go_from (a,b) (path @ [(a,b)]))
		|[(a,b);(c,d)]->
			(try (go_from (a,b) (path @ [(a,b)])) with
				|KeepLooking -> go_from (c,d) (path @ [(c,d)])
			)
		|[(a,b);(c,d);(e,f)]->
			(try go_from (a,b) (path @ [(a,b)]) with 
			|KeepLooking ->
				(try go_from (b,c) (path @ [(b,c)]) with 
				|KeepLooking -> go_from (e,f) (path @ [(e,f)])
				)
			)	
		|_ -> raise (Failure ("Not possible to have 4 direction to go"))
		in try go_from (2,3) [(2,3)] with
		|KeepLooking -> None
		
