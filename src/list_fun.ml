(** 
	General use functions
	@author ocaml-n00b
 *)


(*  *)
let rec unite a b = function
  | [] -> []
  | h::t -> [(a, b, h)] @ unite a b t;;

let comb l1 l2 l3 = let rec aux n1 acc = function
  | [] -> acc
  | h::t as l-> if n1=0 then aux ((List.length l1)-1) (acc@(unite (List.nth l1 0) h l3)) t
  else aux (n1-1) (acc@(unite (List.nth l1 n1) h l3)) l
in aux ((List.length l1)-1) [] l2;;

(* Remove nth item from a list *)
let rmv_missing l n = 
	let rec aux m acc = function
	  | [] -> []
	  | h::t -> if m=n then (List.rev acc)@t else aux (m+1) (h :: acc) t
	in aux 0 [] l

(* Return: random permutation of n values of a list and its remainder (non-random) *)
let permute_deck tmpdeck n = 
	let rec aux n acc = function
	  | [] -> (acc,[])
	  | _::_ as l -> if n>0 then 
	   let rnd = Random.int (List.length l) in
	   let nlist = rmv_missing l rnd in
	   aux (n-1) ((List.nth l rnd) :: acc) nlist 
	  else  ( acc , l)
	in aux n [] tmpdeck;;
	
(* Return a tuple with (a list of random elements of deck with length divisable by np ,and a lis of the the remaining elements) *)
let deal_gems deck np = 
	let rec aux m acc l =
		if m>0 then let (x,y) = permute_deck l (35/np) in
			aux (m-1) (acc@[x]) y
		else (acc, l)
	in aux np [] deck;;
	
(* Get a list of int from 0 to b *)
let int_list b = 
	let rec aux acc a = 
		if a<b then aux (acc@[a]) (a+1) else acc
	in aux [] 0;;
