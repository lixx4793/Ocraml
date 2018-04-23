let circle_area_v1 =fun dia ->0.25*. 3.1415 *. dia*.dia
let circle_area_v2 = fun dia -> let pi = 3.14 in 0.25*.dia*.dia*.pi
let rec product xs =
	match xs with |[] ->1 | x::rest ->x* product rest

let rec sum_diffs xs=
	match xs with
	| x1 :: (x2::[]) ->(x1 -x2)
	|[] ->0
	|x1::[] ->0
	|x::x1::rest ->(x-x1)+ sum_diffs (x1::rest)


let distance (x,y) (x1,y1) =
sqrt((x-.x1)*.(x-.x1)+.(y-.y1)*.(y-.y1))

let triangle_perimeter (x1,y1) (x2,y2) (x3,y3)=
	distance (x1,y1) (x2,y2) +. distance (x2,y2) (x3, y3) +. distance(x1,y1) (x3,y3)

