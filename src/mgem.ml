(* #!/usr/bin/ocaml 

#use "common_def.ml";;


(* load other libraries *)
#use "list_fun.ml"
#use "./search_deck.ml"
#use "./sheet.ml"

#use "player.ml"
*)


Random.self_init ();;
open Common_def

(* command line arguments *)
let gstatus = let np = ref 3 in
let nhump = ref 1 in
let ai = ref 0 in
Arg.parse [("-p",Arg.Int (fun a -> np:=a), "Set the number of players.");
("-n",Arg.Int (fun a -> nhump:=a), "Set the number of human players.");
("-d",Arg.Int (fun a -> ai:=a), "Set the dificulty level.")]
print_endline "\027[32mA clone of the card game Sleuth(R).\027[39m\nThe available options are:";
{ turn=0; humplyrs= !nhump; nplyrs= !np; ai_lvl= !ai };;


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

let (pscards, tmp_deck) = 
let sdeck = Search_deck.make_sdeck gem typ color in
List_fun.permute_deck sdeck (4*gstatus.nplyrs);;

let draw_deck = let (a,_) = List_fun.permute_deck tmp_deck (List.length tmp_deck) in
{cards= a};;
let used_deck = {cards=[]};;

let (missing_gem, rdeck) =
	let gdeck = List_fun.comb gem typ color in
	let nrnd = Random.int 36 in
	(List.nth gdeck nrnd, List_fun.rmv_missing gdeck nrnd) ;;

let plyrs = 
	let (plyrs_hands, disc) = (List_fun.deal_gems rdeck gstatus.nplyrs) in
	let info = Sheet.sht_init() in
	let dsheet = Sheet.fill_sheet disc info in
	let gplyrs = Sheet.get_plyrs_sheets plyrs_hands dsheet in
	let tmp_hand = Search_deck.add_scards gplyrs pscards in
	Player.init_plyrs tmp_hand;;

(* Get new search card remove old *)
(* TODO: Move to search_deck.ml after moving the draw_deck there
*)
let new_scrd nc np = 
	let p = List.nth plyrs np in
	let newy = [List.hd draw_deck.cards]@(List_fun.rmv_missing p.scards nc) in
	draw_deck.cards <- List.tl draw_deck.cards ;
	used_deck.cards <- [(List.nth p.scards nc)]@used_deck.cards ;
	p.scards <- newy ;;

let print_hand () = 
	let p = (List.nth plyrs (gstatus.turn mod gstatus.nplyrs)) in
	Sheet.prnt_sheet p.sheet color; print_endline "\nSearch Cards:\n--------------";
	List.iter (fun a -> let (x,y) = a in
	Printf.printf "[%s],[%s]\n" x y) p.scards ;
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	()

let print_table_search_cards () = 
	Printf.printf "All search card on the table:";
	List.iter2 (fun a n -> let p = a in
	Printf.printf "\nPlayer %d\n" (n+1);
	List.iter (fun b -> let (x, y) = b in
	Printf.printf "[%s],[%s]\n" x y) p.scards) plyrs (List_fun.int_list gstatus.nplyrs) ;
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	()
	
let play_search_card () = 
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
	new_scrd card (gstatus.turn mod gstatus.nplyrs) ;
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	gstatus.turn <- gstatus.turn + 1

let change_search_cards () =
	new_scrd 3 (gstatus.turn mod gstatus.nplyrs); 
	new_scrd 3 (gstatus.turn mod gstatus.nplyrs); 
	new_scrd 3 (gstatus.turn mod gstatus.nplyrs); 
	new_scrd 3 (gstatus.turn mod gstatus.nplyrs);
	print_endline "Your search cards have been changed!";
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	gstatus.turn <- gstatus.turn + 1

let view_played_cards () = 
	Printf.printf "All search card on the table:";
	List.iter2 (fun a n -> let p = a in
	Printf.printf "\nPlayer %d\n" (n+1);
	List.iter (Printf.printf "%s\n") p.info) plyrs (List_fun.int_list gstatus.nplyrs) ;
	print_string "\n[Press Enter to Continue]" ; let _ = read_line () in
	()

let guess_missing_gem () = 
	let guess = get_guess () in
	let (x, y, z) = missing_gem in
	if guess = missing_gem then print_string "You Won! The answer IS "
	else print_string "You Lost! The correct answer was ";
	Printf.printf "([%s],[%s],[%s])!\n\n" x y z;
	flush stdout ;
	raise (Quit ())

(* Start Game Mech *)
let cmnd = function
	| "A" | "a" -> print_hand ()
	| "B" | "b" -> print_table_search_cards ()
	| "C" | "c" -> play_search_card ()
	| "D" | "d" -> change_search_cards ()
	| "E" | "e" -> view_played_cards ()
	| "F" | "f" -> guess_missing_gem ()
	| "quit" | "exit" | "q" -> raise (Quit ())
	| _ -> ();;

(* Sorry excuse for a computer player (Randomly play search cards) *)
let ai_0 cplyr = 
  let card = Random.int 4 in
  let t = (List.nth plyrs cplyr).scards in
  let tup = List.nth t card in
  let plyr = ref (Random.int (gstatus.nplyrs-1)) in
  if !plyr = cplyr then plyr := (gstatus.nplyrs-1);
  let this = Player.chk_gems (List.nth plyrs !plyr) (Search_deck.fnd_scrd tup ~ain:(-1)) in
  let nsheet = (List.nth plyrs cplyr).sheet in
  (List.nth plyrs cplyr).sheet <- Sheet.fill_sheet this nsheet ~chr:(char_of_int (!plyr+49));
  new_scrd card cplyr ;
  gstatus.turn <- gstatus.turn + 1;;

(* Game Loop *)
let rec main_loop () =
  (if (gstatus.turn mod gstatus.nplyrs) < gstatus.humplyrs then
    (Printf.printf "\nIt's player %ds turn.\n" ((gstatus.turn mod gstatus.nplyrs)+1) ;
    print_string "Choose an option:\n-------------------\nA:View My Hand\nB:Search Cards on the table\nC:Play Search Card\nD:Change Search Cards\nE:View gathered intelligence\nF:Guess Missing Gem\n\n> ";
    try cmnd (read_line ()) with Invalid_argument "List.nth" -> ())
  else ai_0 (gstatus.turn mod gstatus.nplyrs) ); 
try main_loop ()
with Quit () -> print_endline "The game has terminated!"
| Failure "int_of_string" -> main_loop ();;

(* Start Game *)
try main_loop () with Failure "int_of_string" -> main_loop ()
| Quit () -> print_endline "The game has terminated!";;

(* --------- TODO -----------
[Make it so that player can't target himself]
[Add multiplayer] (on same computer, must add online?)
Add AI ->
  [1. Change sheet to matrix for easy computer analysis] (string list not matrix)
  [2. Make print matrix function] (for string list)
  3. Make info computer friendly
  4. Design strategy for AI
--------- TODO ----------- *)

