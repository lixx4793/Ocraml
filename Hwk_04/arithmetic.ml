
type expr 
  = Const of int
  | Add of  expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr


let rec show_expr expr =
match expr with
|Const n->""^ string_of_int n
|Add(exp1,exp2) -> "(" ^ (show_expr exp1) ^ "+" ^ (show_expr exp2)^")"
|Mul(exp1,exp2) -> "(" ^ (show_expr exp1) ^ "*" ^ (show_expr exp2) ^ ")"
|Sub(exp1,exp2) -> "(" ^ (show_expr exp1) ^ "-" ^ (show_expr exp2) ^ ")"
|Div(exp1,exp2) -> "(" ^ (show_expr exp1) ^ "/" ^ (show_expr exp2) ^ ")"

(*expr ->int*)
let operator expr  =
match expr with
|Const n->2 
|Add(expr1,expr) -> 0
|Sub(expr1,expr) ->0
|Mul(expr,exp)->1
|Div(expr,exp)->1


let  show_pretty_expr expr =
let  rec  helper current  expr =
match expr with
|Const n->
 	string_of_int n
|Add(expr1, expr2) ->
	if( operator expr2 =0)
		 then  (helper (operator expr1) expr1) ^"+("^ (helper (operator expr2) expr2)^")"
	else (helper (operator expr1)  expr1) ^"+"^ (helper (operator expr2) expr2)
|Sub(exp1,exp2) ->
	if (operator exp2= 0)
		then  helper (operator exp1) exp1 ^"-("^ (helper (operator exp2) exp2)^")"
	else helper (operator exp1) exp1 ^"-"^ (helper (operator exp2) exp2)
|Mul(exp1,exp2) -> 
	if (operator exp2 < 2) 
		then helper (operator exp1) exp1 ^ "*(" ^(helper (operator exp2) exp2)^")"
	else helper (operator exp1) exp1 ^"*" ^(helper (operator exp2) exp2)
|Div(exp1,exp2) ->
	if (operator exp2< 2)
		then  helper (operator exp1) exp1 ^ "/" ^(helper (operator exp2) exp2)
	else helper (operator exp1) exp1 ^"/"^ (helper (operator exp2) exp2)
in 
match expr with
|Const n ->string_of_int n
|Add(exp1,exp2)-> helper 0 expr
|Sub(exp1,exp2)-> helper 0 expr
|Mul(exp1,exp2)-> helper 1 expr
|Div(exp1,exp2)-> helper 1 expr
	
