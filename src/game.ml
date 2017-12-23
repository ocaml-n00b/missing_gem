open Common_def

let new_scrd plyrs draw_deck used_deck nc np = 
	let p = List.nth plyrs np in
	let newy = [List.hd draw_deck.cards]@(List_fun.rmv_missing p.scards nc) in
	draw_deck.cards <- List.tl draw_deck.cards ;
	used_deck.cards <- [(List.nth p.scards nc)]@used_deck.cards ;
	p.scards <- newy ;;

(* Try Guessing *)
let get_guess () =
  List.iter2 (fun a -> Printf.printf "%d. %s\n" (a+1)) (List_fun.int_list 3) gem;
  print_string "Guess Gem: ";
  let x = List.nth gem ((List_fun.get_int2 (read_line ()) 3) -1) in
  List.iter2 (fun a -> Printf.printf "%d. %s\n" (a+1)) (List_fun.int_list 3) typ;
  print_string "Guess Type: ";
  let y = List.nth typ ((List_fun.get_int2 (read_line ()) 3) -1) in
  List.iter2 (fun a -> Printf.printf "%d. %s\n" (a+1)) (List_fun.int_list 4) color;
  print_string "Guess Color: ";
  let z = List.nth color ((List_fun.get_int2 (read_line ()) 4) -1) in
  (x,y,z);;

let print_hand gstatus plyrs = 
	let p = (List.nth plyrs (gstatus.turn mod gstatus.nplyrs)) in
	Sheet.prnt_sheet p.sheet color; print_endline "\nSearch Cards:\n--------------";
	List.iter (fun a -> let (x,y) = a in
	Printf.printf "[%s],[%s]\n" x y) p.scards ;
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	()

let print_table_search_cards gstatus plyrs = 
	Printf.printf "All search cards on the table:";
	List.iter2 (fun a n -> let p = a in
	Printf.printf "\nPlayer %d\n" (n+1);
	List.iter (fun b -> let (x, y) = b in
	Printf.printf "[%s],[%s]\n" x y) p.scards) plyrs (List_fun.int_list gstatus.nplyrs) ;
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	()
	
let play_search_card gstatus plyrs draw_deck used_deck = 
	let cplyr = (gstatus.turn mod gstatus.nplyrs) in
	let t = (List.nth plyrs cplyr).scards in
	Printf.printf "\nSearch Cards in hand:\n" ;
	List.iter2 (fun a b -> let (x,y) = a in
	Printf.printf "%d: [%s],[%s]\n" (b+1) x y) t (List_fun.int_list 4);
	Printf.printf "Choose a card: "; 
	let card = ((List_fun.get_int2 (read_line ()) 4) -1) in
	print_string "Choose player: " ; 
	let tup = List.nth t card in
	let plyr = ((List_fun.get_int2 (read_line ()) gstatus.nplyrs ~cplyr: (cplyr+1)) -1) in
	let this = Player.chk_gems (List.nth plyrs plyr) (Search_deck.fnd_scrd tup) in
	let nsheet = (List.nth plyrs cplyr).sheet in
	(List.nth plyrs cplyr).sheet <- Sheet.fill_sheet this nsheet ~chr:(char_of_int (plyr+49));
	new_scrd plyrs draw_deck used_deck card (gstatus.turn mod gstatus.nplyrs) ;
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	gstatus.turn <- gstatus.turn + 1

let change_search_cards gstatus plyrs draw_deck used_deck =
	new_scrd plyrs draw_deck used_deck 3 (gstatus.turn mod gstatus.nplyrs); 
	new_scrd plyrs draw_deck used_deck 3 (gstatus.turn mod gstatus.nplyrs); 
	new_scrd plyrs draw_deck used_deck 3 (gstatus.turn mod gstatus.nplyrs); 
	new_scrd plyrs draw_deck used_deck 3 (gstatus.turn mod gstatus.nplyrs);
	print_endline "Your search cards have been changed!";
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	gstatus.turn <- gstatus.turn + 1

let view_played_cards gstatus plyrs = 
	Printf.printf "All search card on the table:";
	List.iter2 (fun a n -> let p = a in
	Printf.printf "\nPlayer %d\n" (n+1);
	List.iter (fun str -> Printf.printf "%s\n" (Bytes.to_string str)) p.info) plyrs (List_fun.int_list gstatus.nplyrs) ;
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	()

let guess_missing_gem missing_gem = 
	let guess = get_guess () in
	let (x, y, z) = missing_gem in
	if guess = missing_gem then print_string "You Won! The answer IS "
	else print_string "You Lost! The correct answer was ";
	Printf.printf "([%s],[%s],[%s])!\n\n" x y z;
	flush stdout ;
	raise (Quit ())
