type value
  = Int of int
  | Bool of bool

type expr = 
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr

  | Lt of expr * expr
  | Eq of expr * expr
  | And of expr * expr

  | Var of string
  | Value of value
  | Mod of expr * expr

type environment = (string * value) list

let rec lookup name env =
  match env with 
  | [ ] -> raise (Failure ("Name \"" ^ name ^ "\" not found."))
  | (k,v)::rest -> if name = k then v else lookup name rest

let rec eval (e: expr) (env: environment) : value = 
  match e with
  | Value v -> v
  | Add (e1, e2) -> 
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (Failure "incompatible types, Add")
     )
  | Sub (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 - v2)
       | _ -> raise (Failure "incompatible types, Sub") 
    )
  | Mul (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 * v2)
       | _ -> raise (Failure "incompatible types, Mul")
     )
  | Div (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 / v2)
       | _ -> raise (Failure "incompatible types, Div")
     )

  | Mod (e1,e2) ->
	(match eval e1 env, eval e2 env with 
	  |Int v1, Int v2 -> Int (v1 mod v2)
	  |_ -> raise(Failure"incompatible types, mod")
	)


  | Lt (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 < v2)
       | _ -> raise (Failure "incompatible types, Lt")
     )
  | Eq (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Bool (v1 = v2)
       | Bool v1, Bool v2 -> Bool (v1 = v2)
       | _ -> raise (Failure "incompatible types, Eq")
     )
  | And (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Bool v1, Bool v2 -> Bool (v1 && v2)
       | _ -> raise (Failure "incompatible types, And")
     )
  | Var n -> lookup n env

type state = environment

type stmt = 
  | Assign of string * expr
  | While of expr * stmt
  | IfThen of expr * stmt 
  | Seq of stmt * stmt
  | ReadNum of string
  | WriteNum of expr
  | IfThenElse of expr * stmt * stmt
  | Skip
 
	

(* x = 2;
   y = x + 3;
   z = y + 2;
   write z
 *)
let program_1 =
  Seq (Assign ("x", Value (Int 2)),
       Seq( Assign ("y", Add (Var "x", Value (Int 3))),
            Seq( Assign ("z", Add (Var "y", Value (Int 2))),
                 WriteNum (Var "z")
               )
          )
      )

(* read x;
   i = 0;
   sum = 0;
   while (i < x) {
     write i;
     sum = sum + i;
     i = i + 1
   }
   write sum
 *)

let program_2 =
  Seq (ReadNum "x",
  Seq (Assign ("i", Value (Int 0)),
  Seq (Assign ("sum", Value (Int 0)),
  Seq (While (Lt (Var "i", Var "x"),
	      Seq (WriteNum (Var "i"),
	      Seq (Assign ("sum", Add (Var "sum", Var "i")),
		   Assign ("i", Add (Var "i", Value (Int 1)))
	          ) ) ),
        WriteNum (Var "sum")
      ) ) ) )


(* program_3
   read x;
   i = 0;
   sum_evens = 0;
   sum_odds = 0;
   while (i < x) {
     write i;
     if i mod 2 = 0 then
        sum_evens = sum_evens + i;
     else
        sum_odds = sum_odds + i;
     i = i + 1
   }
   write sum_evens;
   write sum_odds
 *)
let program_3 =
 Seq ( ReadNum "x",
 Seq ( Assign("i", Value(Int 0)),
 Seq ( Assign("sum_evens", Value (Int 0)),
 Seq ( Assign("sum_odds", Value (Int 0)),
 Seq ( 
	While (Lt(Var "i",Var "x"), 
	
		Seq ( WriteNum (Var "i"), 
		Seq ( 
			IfThenElse ( Eq(Value (Int 0), Mod(Var "i", Value (Int 2))), 
			Assign("sum_evens", Add(Var "sum_evens", Var "i") ) ,
			Assign("sum_odds", Add(Var "sum_odds", Var "i"))
			),
		Assign("i", Add(Var "i", Value (Int(1)) ))))),
		
   Seq(WriteNum ( Var "sum_evens"),  WriteNum(Var "sum_odds")))))))
		







let rec read_number () =
  print_endline "Enter an integer value:" ;
  try int_of_string (read_line ()) with
  | Failure _ -> read_number ()

let write_number n = print_endline (string_of_int n)

let rec exec (s :stmt) (stt: state) : state =
  match s with
  | Assign (v, e) ->  (v, (eval e stt)) :: stt

  | Seq (s1, s2) -> exec s2 (exec s1 stt)
  | WriteNum e -> (match eval e stt with
                   | Int v -> write_number v; stt
                   | _ -> raise (Failure "Only numeric values can be printed.")
                  )
  | IfThenElse(cond, st1, st2) -> (match eval cond stt with
			   |Bool true -> exec st1 stt
			   |Bool false->exec st2 stt
			   |_->raise(Failure "need a boolean value")
				   )

  |IfThen (cond,stat) ->
	(match eval cond stt with
		|Bool true-> exec stat stt
		|Bool false->stt
		|_ ->raise (Failure "need a boolean value")
	)
	
	
	
	
  | ReadNum v -> (v, Int (read_number ())) :: stt

  | While (cond, body) -> (match eval cond stt with
                           | Bool true -> exec (While (cond, body)) 
                                               (exec body stt)
                             (* the version in the Sec_01 directory is slighlty
                                different, but achieves the same results. *)
                           | Bool false -> stt
                          )
  |Skip -> stt
			
   
                          
                         
  let num_sums =11                        
let val_sum_evens =56_
let val_sum_odds =49
let num_sum_evens =9
let num_sum_odds =8

let program_3_test=
Seq ( Assign("x", Value (Int 12)),
 Seq ( Assign("i", Value(Int 0)),
 Seq ( Assign("sum_evens", Value (Int 0)),
 Seq ( Assign("sum_odds", Value (Int 0)),
 Seq ( 
	While (Lt(Var "i",Var "x"), 
	
		Seq ( WriteNum (Var "i"), 
		Seq ( 
			IfThenElse ( Eq(Value (Int 0), Mod(Var "i", Value (Int 2))), 
			Assign("sum_evens", Add(Var "sum_evens", Var "i") ) ,
			Assign("sum_odds", Add(Var "sum_odds", Var "i"))
			),
		Assign("i", Add(Var "i", Value (Int(1)) ))))),
		
   Seq(WriteNum ( Var "sum_evens"),  WriteNum(Var "sum_odds")))))))

let autoCheck = lookup "sum_evens" (exec program_3_test [])



(* Program 4
	
	y = 0;
   if x mod 2 = 0 then y = y + 2;
   if x mod 3 = 0 then y = y + 3;
   if x mod 4 = 0 then y = y + 4;
  *)

let program_4 = 
Seq( Assign( "y", Value (Int 0)),
Seq( 
	IfThen(Eq(Value (Int 0), Mod(Var"x",Value (Int 2))),
		Assign("y", Add(Var "y", Value (Int 2)))),
Seq(
	IfThen(Eq(Value (Int 0), Mod(Var"x", Value (Int 3))),
		Assign("y", Add(Var "y", Value (Int 3)))),
	IfThen(Eq(Value (Int 0), Mod(Var"x", Value (Int 4))),
		Assign("y", Add(Var "y", Value (Int 4)))))))
























