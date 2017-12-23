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
	let dsheet = Sheet.fill_sheet ~chr:('D') disc info in
	let gplyrs = Sheet.get_plyrs_sheets plyrs_hands dsheet in
	let tmp_hand = Search_deck.add_scards gplyrs pscards in
	Player.init_plyrs tmp_hand;;

(* Start Game Mech *)
let cmnd = function
	| "A" | "a" -> Game.print_hand gstatus plyrs
	| "B" | "b" -> Game.print_table_search_cards gstatus plyrs
	| "C" | "c" -> Game.play_search_card gstatus plyrs draw_deck used_deck
	| "D" | "d" -> Game.change_search_cards gstatus plyrs draw_deck used_deck
	| "E" | "e" -> Game.view_played_cards gstatus plyrs
	| "F" | "f" -> Game.guess_missing_gem missing_gem
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
  Game.new_scrd plyrs draw_deck used_deck card cplyr ;
  gstatus.turn <- gstatus.turn + 1;;

(* Game Loop *)
let rec main_loop () =
  (if (gstatus.turn mod gstatus.nplyrs) < gstatus.humplyrs then
    (Printf.printf "\nIt's player %ds turn.\n" ((gstatus.turn mod gstatus.nplyrs)+1) ;
    print_string "Choose an option:\n-------------------\nA:View My Hand\nB:Search Cards on the table\nC:Play Search Card\nD:Change Search Cards\nE:View gathered intelligence\nF:Guess Missing Gem\n\n> ";
    try cmnd (read_line ()) with Invalid_argument _ -> ())
  else ai_0 (gstatus.turn mod gstatus.nplyrs) ); 
try main_loop ()
with Quit () -> print_endline "The game has terminated!"
| Failure _ -> main_loop ();;

(* Start Game *)
try main_loop () with Failure _ -> main_loop ()
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

