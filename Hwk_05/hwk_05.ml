(*Part two*)
let rec ands l =
match l with 
|[] ->true
|x::xs ->if x then ands xs else false



