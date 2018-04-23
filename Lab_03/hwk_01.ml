(*Lab 03  work with Jack Hanson*)
(* hw question 6*)
let rec max_list ls =
        let bigger x y = if x>=y then x else y in
        match ls with
        | [] ->  raise (Failure "No maxium in the empty list")
        | x1::[] ->x1
        | x1::rest ->bigger x1 (max_list rest)

        (* restriction: all elements in list should be in the same data type*)




(* hw question 7*)
let rec drop num ls =      
        if 0 >= num then ls
        else
        match ls with
        |[] ->[]
        |x1::rest ->drop (num-1) rest


let rec matrix_scalar_add ls incr =
	let rec helper11 ls incr = 
	match ls with
	|[] ->[]
	|x1::rest ->( (x1+incr) :: (helper11 rest incr)) in
        match ls with
        |[] -> []
        |x1 :: rest ->( helper11 x1 incr :: matrix_scalar_add rest incr)





















	
(*hw question 1 *)
let even input = 
	if input mod 2 = 0 then true
	else false


(*hw question 2 *)
let rec euclid a b =
	if a =b then a
	else if a < b then euclid a (b-a) else
	euclid (a-b) b



(*hw question 3*)
let frac_add (x1,y1) (x2,y2) =
	((x1*y2+x2*y1),(y1*y2))


(*hw question 4*)
let frac_simplify (x1, y1)=
	let comDiv = euclid x1 y1 in
	(x1/comDiv,y1/comDiv)
	
(* hw question 5*)
let square_approx n accu = 
	let num = n in 
	let rec helper lower upper =
	let guess = 0.5 *.(lower+.upper) in	if (upper-.lower) <= accu then (lower, upper) else
	if (guess*.guess) >num then helper lower guess 
	else helper guess upper 
	in helper 1.0 n
		
	

	
	
	
	
	

let rec length ls =
	match ls with
	| [] ->0	| x1 ::rest -> 1+length(rest)
	
let hd ls =
	match ls with
	|[] -> raise(Failure "can't be empty")
	|x1 :: rest -> x1
	
	
	
(* hw question8 *)
let rec rev  ls2 =
	match ls2 with
	|[] ->[]
	| x1::rest -> ((rev  rest)@[x1])
		
	
	
	
(*hw question 9*)
let perimeter ls =
	let distance (x,y)(x2,y2) = sqrt((x-.x2)*.(x-.x2) +.(y-.y2)*.(y-.y2)) in
	let hd =hd ls in
	let rec helper helperls =
	match helperls with
	|[] -> 0.0	|(x1,y1)::[] -> 0.0
	|(x1,y1)::(x2,y2) ::[] -> distance(x1,y1)(x2,y2)+.distance hd (x2,y2)
	|(x1,y1) ::(x2,y2):: rest -> distance(x1,y1)(x2,y2) +. helper ((x2,y2)::rest)
	in helper ls
	
(*hw question 10 *)
let rec is_matrix ls =
	match ls with
	|[] -> true
	|x1::[]->true
	|x1::x2::rest ->((length x1=length x2) && (is_matrix(x2::rest)))
	
	
	
	
	
(* Bonus round*)
let rec getfirst ls =
	match ls with 
	|[] -> []
	|x ::rest ->( hd x :: getfirst rest)

let del ls =
	match ls with
	| [] -> []
	| x:: rest -> rest

let rec restlis ls =
	match ls with
	|[] -> []
	| x ::rest -> (del x)::(restlis rest)


let rec body ls  =
	let flag = length (hd ls)in
	let next = restlis ls in
	if 0 = flag then [] else
	(getfirst ls)::(body next)

let matrix_transpose ls =
	body ls 

