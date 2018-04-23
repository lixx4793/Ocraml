module type Arithmetic = sig
	type s
	val to_String: s -> string
	val add: s-> s -> s
	val mul: s-> s -> s
	val sum: s list -> s list -> s
	end 



	

module type Arithmetic_intf = sig
	type s
	type endPoint
	val scalar_add: 		s -> endPoint -> endPoint
	val scalar_mul: 		s -> endPoint -> endPoint
	val create: 			int -> s -> endPoint 
	val from_list: 			s list -> endPoint
	val to_list: 			endPoint -> s list
	val scalar_prod: 		endPoint -> endPoint-> s option
	val to_string :			endPoint -> string
	val size:				endPoint -> int
end	




module Make_vector(Ttype: Arithmetic):
					(Arithmetic_intf with  type s = Ttype.s)= struct
type s = Ttype.s
type endPoint = Ttype.s list
				
				
let  rec create num vec =
if (num<0) then raise(Failure "Stop playing it")
else
match num with 
|0 -> []
|_ ->vec :: create (num-1) vec

let scalar_add vec endp =
List.map (Ttype.add vec) endp

let scalar_mul vec endp =
List.map (Ttype.mul vec) endp

let from_list ls =	ls
let to_list endp = endp

let size endP = 
List.fold_left (fun x y-> 1+x) 0 endP


let scalar_prod endP1 endP2 =
if (size endP1) <> (size endP2) then None
else Some (Ttype.sum endP1 endP2)

let to_string endP = 
let rec helper2 ls=
match ls with
|[]->""
|x::[] -> Ttype.to_String x
|x::xs ->Ttype.to_String x ^", "^helper2 xs 
in

"<< "^ (string_of_int (size endP)) ^" | "^ 
helper2 endP ^ " >>"


	
	
end




(***************************************************************************)
module Int_arithmetic: (Arithmetic with  type s=int)= struct
type s =int 
let to_String x = string_of_int x 
let add x y = x + y
let mul x y = x * y
let rec sum s1 s2 = 
match (s1,s2) with
|([],[]) -> 0
|((x::xs),(y::ys)) -> x*y + sum xs ys

end

module Int_vector = Make_vector(Int_arithmetic)



(*******************************************************************************)


module Complex_arithmetic: (Arithmetic with type s=float*float )= struct
type s = float * float


let to_String x =
	let f flo =
	if flo>=0.0 then "+"^string_of_float flo
	else "-"^string_of_float flo
	in
match x with
|(f1,f2) ->"("^string_of_float f1^ (f f2)^"i" ^")"
|_->raise(Failure "I don't think this going to happen")


let add x y = 
match (x,y) with
|((x1,y1),(x2,y2)) ->((x1+.x2),(y1+.y2))

let mul x y =
match (x,y) with
|((x1,y1),(x2,y2)) ->((x1*.x2-.y1*.y2),(x1*.y2+.y1*.x2))

let rec sum x y =
match (x,y) with
|(x::[],y::[]) -> mul x y
|((x::xs),(y::ys)) -> add (mul x y) (sum xs ys)


end

module Complex_vector = Make_vector(Complex_arithmetic)

let v1 = Int_vector.create 10 1

let v2 = Int_vector.from_list [1;2;3;4;5]

let v3 = Int_vector.scalar_add 3 v2

let v4 = Int_vector.scalar_mul 10 v2

let i1 = Int_vector.scalar_prod v3 v4

let l1 = Int_vector.to_list v3 

let i2 = Int_vector.size v4

let s1 = Int_vector.to_string v1

let s2 = Int_vector.to_string v2

let s3 = Int_vector.to_string v3

let s4 = Int_vector.to_string v4




let v5 = Complex_vector.from_list [ (1.0, 2.0); (3.0, 4.0); (5.0, 6.0) ]

let v6 = Complex_vector.scalar_add (5.0, 5.0) v5

let c1 = Complex_vector.scalar_prod v5 v6

let s5 = Complex_vector.to_string v5

let s6 = Complex_vector.to_string v6



