open Common_def

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
		let (y, tmp) = List_fun.permute_deck l 4 in
		aux (acc@[(x, y, z)]) tmp t
	in aux [] sdck plyrl;;

(* Return a list of elements of categories of cards not provided *)
let choose_elem = function
  | "Diamond" | "Pearl" | "Opal" -> ["One-Element"]@typ@color
  | "Solitaire" | "Pairs" | "Cluster" -> ["One-Element"]@gem@color
  | "Red" | "Blue" | "Green" | "Yellow" -> ["One-Element"]@gem@typ
  | "Free-Choice" -> gem@typ@color
  | _ -> [];;

(* Return a triple with (gem, typ, color) of the given search card *)
let rec fnd_scrd ?(ain= -2) tup =
	let (x, y) = tup in
	match tup with
	  | (("Diamond" | "Pearl" | "Opal") , ("Solitaire" | "Pairs" | "Cluster")) -> (x, y, "")
	  | (("Diamond" | "Pearl" | "Opal") , ("Red" | "Blue" | "Green" | "Yellow")) -> (x, "", y)
	  | (("Solitaire" | "Pairs" | "Cluster") , ("Red" | "Blue" | "Green" | "Yellow")) -> ("", x, y)
	  | ("One-Element" , ("Diamond" | "Pearl" | "Opal") ) -> (y, "", "")
	  | ("One-Element" , ("Solitaire" | "Pairs" | "Cluster")) -> ("", y, "")
	  | ("One-Element" , ("Red" | "Blue" | "Green" | "Yellow")) -> ("", "", y)
	  | ("Free-Choice", _) -> let tmpl = choose_elem y in
		  let ntmpl = List.length tmpl in (
		  if ain < -1 then
			(List.iter2 (fun a b -> Printf.printf "%d. %s\n" (b+1) a) tmpl (List_fun.int_list ntmpl) ;
			print_string "\nChoose a type: " ; let rl = List_fun.get_int2 (read_line ()) ntmpl in
			fnd_scrd (List.nth tmpl (rl-1), y) )
		  else fnd_scrd (List.nth tmpl (Random.int ntmpl), y) )
	  | _ -> fnd_scrd (y, x)

