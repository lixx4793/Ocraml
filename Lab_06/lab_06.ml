
 type 'a tree = Leaf of 'a
		| Fork of 'a * 'a tree * 'a tree

let rec t_size tree = 
match tree with 
|Leaf a->1
|Fork(a,Leaf b, Leaf c) ->3
|Fork(a, Leaf b, Fork(c,t1,t2))->2+ t_size (Fork(c,t1,t2))
|Fork(a,Fork(b,t1,t2),Leaf c )->2 + t_size(Fork(b,t1,t2))
|Fork(a,Fork(b,t1,t2),Fork(c,t3,t4)) -> 1 + t_size (Fork(b,t1,t2))+t_size(Fork(c,t3,t4))

let rec t_sum tree=
match tree with
|Leaf a ->a
|Fork(a,Leaf b, Leaf c) ->a+b+c
|Fork(a, Leaf b, Fork(c,t1,t2))->a+b+ t_sum (Fork(c,t1,t2))
|Fork(a,Fork(b,t1,t2),Leaf c )->a+c+ t_sum(Fork(b,t1,t2))
|Fork(a,Fork(b,t1,t2),Fork(c,t3,t4)) -> a + t_sum (Fork(b,t1,t2))+t_sum(Fork(c,t3,t4))

let rec t_charcount tree =
match tree with
|Leaf a ->String.length a
|Fork(a,Leaf b, Leaf c) ->String.length (a^b^c)
|Fork(a, Leaf b, Fork(c,t1,t2))->String.length(a^b)+ t_charcount (Fork(c,t1,t2))
|Fork(a,Fork(b,t1,t2),Leaf c )->String.length (a^c) + t_charcount(Fork(b,t1,t2))
|Fork(a,Fork(b,t1,t2),Fork(c,t3,t4)) -> (String.length a)  + t_charcount (Fork(b,t1,t2))+t_charcount(Fork(c,t3,t4))

let rec t_concat tree =
match tree with 
|Leaf a ->a
|Fork(a,Leaf b, Leaf c) ->a^b^c
|Fork(a, Leaf b, Fork(c,t1,t2))->a^b^(t_concat(Fork(c,t1,t2)))
|Fork(a,Fork(b,t1,t2),Leaf c )->a^c^ (t_concat(Fork(b,t1,t2)))
|Fork(a,Fork(b,t1,t2),Fork(c,t3,t4)) ->a^ (t_concat (Fork(b,t1,t2)))^(t_concat(Fork(c,t3,t4)))





(*Part 2*)
let rec t_opt_size tree =
let f a = match a with
|None ->0
|Some x->1
in
match tree with
|Leaf a->f a
|Fork(a,Leaf b, Leaf c) ->(f a) + (f b ) + (f c)
|Fork(a, Leaf b, Fork(c,t1,t2))->(f a)+(f b)+ t_opt_size (Fork(c,t1,t2))
|Fork(a,Fork(b,t1,t2),Leaf c )->(f a)+(f c) + t_opt_size(Fork(b,t1,t2))
|Fork(a,Fork(b,t1,t2),Fork(c,t3,t4)) -> (f a) + t_opt_size (Fork(b,t1,t2))+t_opt_size(Fork(c,t3,t4))




let rec t_opt_sum tree=
let f a =
match a with
|None -> 0
|Some x ->x in
match tree with
|Leaf a ->f a
|Fork(a,Leaf b, Leaf c) ->(f a)+ (f b)+ (f c)
|Fork(a, Leaf b, Fork(c,t1,t2))->(f a)+(f b)+ t_opt_sum (Fork(c,t1,t2))
|Fork(a,Fork(b,t1,t2),Leaf c )->(f a)+(f c)+ t_opt_sum(Fork(b,t1,t2))
|Fork(a,Fork(b,t1,t2),Fork(c,t3,t4)) -> (f a) + t_opt_sum (Fork(b,t1,t2))+t_opt_sum(Fork(c,t3,t4))


let rec t_opt_charcount tree =
let f a=
match a with 
|None -> 0
|Some x -> String.length x
in
match tree with
|Leaf a->f a
|Fork(a,Leaf b, Leaf c) ->(f a) + (f b) + (f c)
|Fork(a, Leaf b, Fork(c,t1,t2))->(f a) + (f b)+ t_opt_charcount (Fork(c,t1,t2))
|Fork(a,Fork(b,t1,t2),Leaf c )->(f a)+(f c) + t_opt_charcount(Fork(b,t1,t2))
|Fork(a,Fork(b,t1,t2),Fork(c,t3,t4)) ->(f a)  + t_opt_charcount (Fork(b,t1,t2))+t_opt_charcount(Fork(c,t3,t4))

let rec t_opt_concat tree =
let f a =
match a with 
|None-> ""
|Some x-> x
in
match tree with
|Leaf a ->f a
|Fork(a,Leaf b, Leaf c) ->(f a)^(f b)^(f c)
|Fork(a, Leaf b, Fork(c,t1,t2))->(f a)^(f b)^(t_opt_concat(Fork(c,t1,t2)))
|Fork(a,Fork(b,t1,t2),Leaf c )->(f a)^(f c)^ (t_opt_concat(Fork(b,t1,t2)))
|Fork(a,Fork(b,t1,t2),Fork(c,t3,t4)) ->(f a)^ (t_opt_concat (Fork(b,t1,t2)))^(t_opt_concat(Fork(c,t3,t4)))








(* Part C*)
let rec tfold (l:'a -> 'b) (f:'a -> 'b -> 'b -> 'b)  (t:'a tree) : 'b = 
         match t with
         | Leaf v -> l v
         | Fork (v, t1, t2) -> f v (tfold l f t1) (tfold l f t2)

let tf_sum tree=
let f x l r = x+ l + r in
tfold (fun x -> x) f tree

let tf_size tree = 
let f x l r = 1 + l + r in
tfold (fun x -> 1) f tree


let tf_char_count tree = 
let f x l r = (String.length x) + l + r in
tfold (fun x -> String.length x) f tree


let tf_concat tree = 
let f x l r = x ^l^r in
tfold (fun x ->x ) f tree

let tf_opt_sum tree =
let f a = 
match a with 
|None -> 0
|Some x -> x
in let f2 x l r =(f x) + l + r in
tfold (fun x-> f x) f2 tree

let tf_opt_size tree =
let f a =
match a with 
|None -> 0
|Some x -> 1 in 
let f2 x l r = (f x) + l + r in 
tfold (fun x-> f x) f2 tree

let tf_opt_char_count tree =
let f a =
match a with 
|None -> 0
|Some x -> String.length x
in
let f2 x l r = (f x) + l + r in 
tfold (fun x -> f x) f2 tree



let tf_opt_concat tree =
let f a =
match a with
|None -> ""
|Some x -> x
in let f2 x l r = (f x) ^ l ^ r in
tfold (fun x-> f x) f2 tree




(*Part D*)
type 'a btree = Empty
|Node of 'a btree * 'a *'a btree


let rec bt_insert_by f ele btree =
match btree with
|Empty -> Node(Empty, ele , Empty)
|Node(tb1, a , tb2) -> if( f ele a)>0 then Node(tb1, a ,bt_insert_by f ele tb2) else
Node(bt_insert_by f ele tb1, a, tb2)


let rec bt_elem_by f ele btree =
let l x y = Pervasives.compare x y in
match btree with
|Empty -> false
|Node(tb1,a,tb2) ->if f ele a then true else
 if(l ele a) > 0 then bt_elem_by f ele tb2 else bt_elem_by f ele tb1


let rec bt_to_list btree = 
match btree with 
|Empty -> []
|Node(tb1,a,tb2) ->  (bt_to_list tb1)@(a::[]) @ (bt_to_list tb2)


let rec btfold l f tree = 
match tree with 
|Empty -> l
|Node(tb1, ele, tb2) -> f (btfold l f tb1)  ele (btfold l f tb2) 


let btf_elem_by f2 ele tree =
let f tb1 a tb2 = f2 a ele || tb1 || tb2 in
btfold  false f tree


let btf_to_list tree =
let f tb1 a tb2 = tb1 @ (a::[]) @ tb2 in 
btfold [] f tree

(*
The insert operation is difficult to write because whe ninsert an elelment n1 in the the current 
n1 need to compare with both the current node and the sub-nodes, which is hard to implemented 
by using tbfold method.

*)
