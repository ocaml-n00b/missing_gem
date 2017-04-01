(* first: 173; +5 ; sec row: +111 *)
let fill_sheet ?(chr='X') l s = 
	let rec aux acc = function
		| [] -> acc
		| h::t -> 
			let (x,y,z) = h in
			let a = match x with
			| "Pearl" -> 0
			| "Opal" -> 3
			| "Diamond" -> 6
			| _ -> 0
			in let b = match y with
			| "Solitaire" -> 0
			| "Pairs" -> 1
			| "Cluster" -> 2
			| _ -> 0
			in let c = match z with
			| "Red" -> 0
			| "Blue" -> 1
			| "Green" -> 2
			| "Yellow" -> 3
			| _ -> 0
		in Bytes.set (List.nth acc c) (a+b) chr;
		aux acc t 
	in aux s l;;

(* Players Hands TODO: make more efficinet use reverse *)
(*  List.map (fun hand -> fill_sheet hand (List.map Bytes.copy sheet)) hands *)
let get_plyrs_sheets hand sheet = 
	let rec aux acc = function
		| [] -> acc
		| h::t -> aux (acc@[(h, (fill_sheet h (List.map Bytes.copy sheet)) )]) t
	in aux [] hand;;

(* Print a users sheet *)
let prnt_sheet s color =
  let lnstr = "\n-----------------------------------------------------" in
  print_string ("Information Sheet:\n\n       |    Pearl     |     Opals    |   Diamonds   |\n       | 1  |  2 |  3 | 1  |  2 |  3 | 1  |  2 |  3 |"^lnstr);
  List.iter2 (fun a b -> Printf.printf "\n%7s|" a; 
  (String.iter (Printf.printf "  %c |" ) b); print_string lnstr) color s;;
  
let sht_init () = ["         "; "         "; "         "; "         "]

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
			(List.iter2 (fun a b -> Printf.printf "%d. %s\n" (b+1) a) tmpl (int_list ntmpl) ;
			print_string "\nChoose a type: " ; let rl = get_int2 (read_line ()) ntmpl in
			fnd_scrd (List.nth tmpl (rl-1), y) )
		  else fnd_scrd (List.nth tmpl (Random.int ntmpl), y) )
	  | _ -> fnd_scrd (y, x)

