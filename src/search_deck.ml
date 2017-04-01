

(* Create Deck *)
let join a l = let rec aux = function
  | [] -> []
  | h::t -> [(a,h)]@aux t
in aux l;;

let join2 l1 l2 = let rec aux = function
  | [] -> []
  | h::t -> (join h l1)@aux t
in aux l2;;

let make_sdeck l1 l2 l3 = 
  let sngls = l1@l2@l3 in
  (join "One-Element" sngls)@(join2 l2 l1)@(join2 l3 l1)@(join2 l3 l2)@(join "Free-Choice" sngls)@[("Free-Choice", "Free-Choice")]  ;;

let add_scards plyrl sdck = 
	let rec aux acc l = function
	  | [] -> acc
	  | h::t -> let (x,z) = h in
		let (y, tmp) = permute_deck l 4 in
		aux (acc@[(x, y, z)]) tmp t
	in aux [] sdck plyrl;;
