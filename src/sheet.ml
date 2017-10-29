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
  print_string ("Information Sheet:\n\n       |    Pearl     |     Opal     |   Diamond    |\n       | 1  |  2 |  3 | 1  |  2 |  3 | 1  |  2 |  3 |"^lnstr);
  List.iter2 (fun a b -> Printf.printf "\n%7s|" a; 
  (Bytes.iter (Printf.printf "  %c |" ) b); print_string lnstr) color s;;
  
let sht_init () = ["         "; "         "; "         "; "         "]


