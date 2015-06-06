
(* General Bindings *)
let gem = ["Diamond" ; "Pearl" ; "Opal"]
let typ = ["Solitaire" ; "Pairs" ; "Cluster"]
let color = ["Red" ; "Blue" ; "Green" ; "Yellow"]

(* Game types & Exceptions *)
type player = {
gems : (string * string * string) list; 
mutable scards : (string * string) list ;
mutable sheet : string list;
mutable info : string list};;

type deck = {mutable cards : (string * string) list};;
type mcard = {mutable card : (string * string * string)};;

type status = {
  mutable turn : int;
  humplyrs : int;
  nplyrs : int;
  ai_lvl : int };;

exception Quit of unit;;
Random.self_init ();;

let gstatus = let np = ref 3 in
let nhump = ref 1 in
let ai = ref 0 in
Arg.parse [("-p",Arg.Int (fun a -> np:=a), "Set the number of players.");
("-n",Arg.Int (fun a -> nhump:=a), "Set the number of human players.");
("-d",Arg.Int (fun a -> ai:=a), "Set the dificulty level.")]
print_endline "A clone of the card game Sleuth(R). \nThe available options are:";
{ turn=0; humplyrs= !nhump; nplyrs= !np; ai_lvl= !ai };;

let rec unite a b = function
  | [] -> []
  | h::t -> [(a, b, h)] @ unite a b t;;

let comb l1 l2 l3 = let rec aux n1 acc = function
  | [] -> acc
  | h::t as l-> if n1=0 then aux ((List.length l1)-1) (acc@(unite (List.nth l1 0) h l3)) t
  else aux (n1-1) (acc@(unite (List.nth l1 n1) h l3)) l
in aux ((List.length l1)-1) [] l2;;


(* Remove nth item from a list *)
let rmv_missing l n = let rec aux m acc = function
  | [] -> []
  | h::t -> if m=n then (List.rev acc)@t else aux (m+1) (h :: acc) t
in aux 0 [] l;;


(* Return: random permutation of n values of a list and its remainder (non-random) *)
let deal_plyr tmpdeck n = let rec aux n acc = function
  | [] -> (acc,[])
  | _::_ as l -> if n>0 then 
   let rnd = Random.int (List.length l) in
   let nlist = rmv_missing l rnd in
   aux (n-1) ((List.nth l rnd) :: acc) nlist 
  else  ( acc , l)
in aux n [] tmpdeck;;
 
let deal_gems deck np = let rec aux m acc l =
 if m>0 then let (x,y) = deal_plyr l (35/np) in
  aux (m-1) (acc@[x]) y
 else (acc, l)
in aux np [] deck;;


(* first: 173; +5 ; sec row: +111 *)

let fill_sheet ?(chr='X') l s = let rec aux acc = function
  | [] -> acc
  | h::t -> let (x,y,z) = h in
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
  in String.set (List.nth acc c) (a+b) chr;
aux acc t in aux s l;;

(* Players Hands *)
let get_plyrs hand sheet = let rec aux acc = function
  | [] -> acc
  | h::t -> aux (acc@[(h, (fill_sheet h (List.map String.copy sheet)) )]) t
in aux [] hand;;


(* Search Deck *)
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

let add_scards plyrl sdck = let rec aux acc l = function
  | [] -> acc
  | h::t -> let (x,z) = h in
    let (y, tmp) = deal_plyr l 4 in
    aux (acc@[(x, y, z)]) tmp t
in aux [] sdck plyrl;;

(* Get a list of int from 0 to b *)
let int_list b = let rec aux acc a = 
  if a<b then aux (acc@[a]) (a+1) else acc
in aux [] 0;;

(* Search Cards Use *)

let rec get_int2 ?(cplyr= -1) str nmax = 
if (String.length str) = 0 then 0 else
  let is_int = String.map (fun a -> if (a>='0' && a<='9') then 'T' else 'F') str in
  if String.contains is_int 'F' then (print_string "Enter number again: " ; 
    get_int2 (read_line ()) nmax ~cplyr: cplyr)
  else if ((int_of_string str)<= nmax)&&((int_of_string str)!= cplyr) then (int_of_string str )
else (Printf.printf "Enter number again (0-%d): " nmax; get_int2 (read_line ()) nmax ~cplyr: cplyr);;

let choose_elem = function
  | "Diamond" | "Pearl" | "Opal" -> ["One-Element"]@typ@color
  | "Solitaire" | "Pairs" | "Cluster" -> ["One-Element"]@gem@color
  | "Red" | "Blue" | "Green" | "Yellow" -> ["One-Element"]@gem@typ
  | "Free-Choice" -> gem@typ@color
  | _ -> [];;

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
  | _ -> fnd_scrd (y, x);;

let chk_gems plyr srch = 
  let g = plyr.gems in
  match srch with  (* For Testing *)
  | (x, "", "") -> let n = List.length (List.filter (fun a -> let (i,_,_)=a in (i=x) ) g) in
    Printf.printf "There are %d %s cards\n" n x;
    plyr.info <- (plyr.info)@[( (string_of_int n)^" "^x^" cards." )];
    print_newline () ; []
  | ("", y, "") -> let n = List.length (List.filter (fun a -> let (_,j,_)=a in (j=y) ) g) in
    Printf.printf "There are %d %s cards\n" n y;
    plyr.info <- (plyr.info)@[( (string_of_int n)^" "^y^" cards.")];
    print_newline () ; []
  | ("", "", z) -> let n = List.length (List.filter (fun a -> let (_,_,k)=a in (k=z) ) g) in
    Printf.printf "There are %d %s cards\n" n z;
    plyr.info <- (plyr.info)@[( (string_of_int n)^" "^z^" cards." )];
    print_newline () ; []
  | (x, y, "") -> let tmp = List.filter (fun a -> let (i,j,_)=a in (i=x && j=y) ) g in
    let tmpstr = ((string_of_int (List.length tmp))^" ["^x^"] ["^y^"] cards." ) in 
    print_endline tmpstr ;
    plyr.info <-( (plyr.info)@[tmpstr] );
    tmp
  | (x, "", z) -> let tmp = List.filter (fun a -> let (i,_,k)=a in (i=x && k=z) ) g in
    let tmpstr = ((string_of_int (List.length tmp))^" ["^z^"] ["^x^"] cards." ) in
    print_endline tmpstr ;
    plyr.info <-( (plyr.info)@[tmpstr] );
    tmp
  | ("", y, z) -> let tmp = List.filter (fun a -> let (_,j,k)=a in (j=y && k=z) ) g in
    let tmpstr = ((string_of_int (List.length tmp))^" ["^z^"] ["^y^"] cards.") in
    print_endline tmpstr ;
    plyr.info <-( (plyr.info)@[tmpstr] );
    tmp
  | _ -> [];;

(* Try Guessing *)
let get_guess () =
  List.iter2 (fun a -> Printf.printf "%d. %s\n" (a+1)) (int_list 3) gem;
  print_string "Guess Gem: ";
  let x = List.nth gem ((get_int2 (read_line ()) 3) -1) in
  List.iter2 (fun a -> Printf.printf "%d. %s\n" (a+1)) (int_list 3) typ;
  print_string "Guess Type: ";
  let y = List.nth typ ((get_int2 (read_line ()) 3) -1) in
  List.iter2 (fun a -> Printf.printf "%d. %s\n" (a+1)) (int_list 4) color;
  print_string "Guess Color: ";
  let z = List.nth color ((get_int2 (read_line ()) 4) -1) in
  (x,y,z);;

(* Get Players Hands*)
let init_plyrs plyrl = let rec aux acc = function
  | [] -> acc
  | h::t -> let (x,y,z) = h in
    aux ([{gems=x; scards=y; sheet=z; info=[]}]@acc) t
in aux [] plyrl;;

(* ---------------- More Bindings ---------------- *)

(* Temp bind used_deck for passing to next phase : let draw_deck, used_deck = *)

let (pscards, tmp_deck) = let sdeck = make_sdeck gem typ color in
deal_plyr sdeck (4*gstatus.nplyrs);;


let draw_deck = let (a,_) = deal_plyr tmp_deck (List.length tmp_deck) in
{cards= a};;
let used_deck = {cards=[]};;

let (missing_gem, rdeck) =
  let gdeck = comb gem typ color in
  let nrnd = Random.int 36 in
  (List.nth gdeck nrnd, rmv_missing gdeck nrnd) ;;

let plyrs = 
  let plyrs_hand = (deal_gems rdeck gstatus.nplyrs) in
  let (plyrs_hands, disc) = plyrs_hand in
  let info = ["         "; "         "; "         "; "         "] in
  let dsheet = fill_sheet disc info in
  let gplyrs = get_plyrs plyrs_hands dsheet in
  let tmp_hand = add_scards gplyrs pscards in
  init_plyrs tmp_hand;;

(* Print a users sheet *)
let prnt_sht s =
  let lnstr = "\n-----------------------------------------------------" in
  print_string ("Information Sheet:\n\n       |    Pearl     |     Opals    |   Diamonds   |\n       | 1  |  2 |  3 | 1  |  2 |  3 | 1  |  2 |  3 |"^lnstr);
  List.iter2 (fun a b -> Printf.printf "\n%7s|" a; 
  (String.iter (Printf.printf "  %c |" ) b); print_string lnstr) color s;;

(* Get new search card remove old *)
let new_scrd nc np = 
  let p = List.nth plyrs np in
  let newy = [List.hd draw_deck.cards]@(rmv_missing p.scards nc) in
  draw_deck.cards <- List.tl draw_deck.cards ;
  used_deck.cards <- [(List.nth p.scards nc)]@used_deck.cards ;
  p.scards <- newy ;;

(* Start Game Mech *)
let cmnd = function
| "A" | "a" -> let p = (List.nth plyrs (gstatus.turn mod gstatus.nplyrs)) in
  prnt_sht p.sheet; print_endline "\nSearch Cards:\n--------------";
  List.iter (fun a -> let (x,y) = a in
  Printf.printf "[%s],[%s]\n" x y) p.scards ;
  print_string "\n[Press Enter to Continue]" ; read_line ();
  ()

| "B" | "b" -> Printf.printf "All search card on the table:";
  List.iter2 (fun a n -> let p = a in
  Printf.printf "\nPlayer %d\n" (n+1);
  List.iter (fun b -> let (x, y) = b in
  Printf.printf "[%s],[%s]\n" x y) p.scards) plyrs (int_list gstatus.nplyrs) ;
  print_string "\n[Press Enter to Continue]" ; read_line () ;
  ()

| "C" | "c" ->  let cplyr = (gstatus.turn mod gstatus.nplyrs) in
  let t = (List.nth plyrs cplyr).scards in
  Printf.printf "\nSearch Cards in hand:\n" ;
  List.iter2 (fun a b -> let (x,y) = a in
  Printf.printf "%d: [%s],[%s]\n" (b+1) x y) t (int_list 4);
  Printf.printf "Choose a card: "; 
  let card = ((get_int2 (read_line ()) 4) -1) in
  print_string "Choose player: " ; 
  let tup = List.nth t card in
  let plyr = ((get_int2 (read_line ()) gstatus.nplyrs ~cplyr: (cplyr+1)) -1) in
  let this = chk_gems (List.nth plyrs plyr) (fnd_scrd tup) in
  let nsheet = (List.nth plyrs cplyr).sheet in
  (List.nth plyrs cplyr).sheet <- fill_sheet this nsheet ~chr:(char_of_int (plyr+49));
  new_scrd card (gstatus.turn mod gstatus.nplyrs) ;
  print_string "\n[Press Enter to Continue]" ; read_line () ;
  gstatus.turn <- gstatus.turn + 1

| "D" | "d" -> 
  new_scrd 3 (gstatus.turn mod gstatus.nplyrs); 
  new_scrd 3 (gstatus.turn mod gstatus.nplyrs); 
  new_scrd 3 (gstatus.turn mod gstatus.nplyrs); 
  new_scrd 3 (gstatus.turn mod gstatus.nplyrs);
  print_endline "Your search cards have been changed!";
  print_string "\n[Press Enter to Continue]" ; read_line () ;
  gstatus.turn <- gstatus.turn + 1

| "E" | "e" -> Printf.printf "All search card on the table:";
  List.iter2 (fun a n -> let p = a in
  Printf.printf "\nPlayer %d\n" (n+1);
  List.iter (Printf.printf "%s\n") p.info) plyrs (int_list gstatus.nplyrs) ;
  print_string "\n[Press Enter to Continue]" ; read_line () ;
  ()

| "F" | "f" -> let guess = get_guess () in
  let (x, y, z) = missing_gem in
  if guess = missing_gem then print_string "You Won! The answer IS "
  else print_string "You Lost! The wright answer was ";
  Printf.printf "([%s],[%s],[%s])!\n\n" x y z;
  flush stdout ;
  raise (Quit ())

| "quit" | "exit" | "q" -> raise (Quit ())
| _ -> ();;

(* Sorry excuse for a computer player (Randomly play search cards) *)
let ai_0 cplyr = 
  let card = Random.int 4 in
  let t = (List.nth plyrs cplyr).scards in
  let tup = List.nth t card in
  let plyr = ref (Random.int (gstatus.nplyrs-1)) in
  if !plyr = cplyr then plyr := (gstatus.nplyrs-1);
  let this = chk_gems (List.nth plyrs !plyr) (fnd_scrd tup ~ain:(-1)) in
  let nsheet = (List.nth plyrs cplyr).sheet in
  (List.nth plyrs cplyr).sheet <- fill_sheet this nsheet ~chr:(char_of_int (!plyr+49));
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

