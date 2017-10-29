(* General Bindings *)
let gem = ["Diamond" ; "Pearl" ; "Opal"]
let typ = ["Solitaire" ; "Pairs" ; "Cluster"]
let color = ["Red" ; "Blue" ; "Green" ; "Yellow"]

(* Game types & Exceptions *)
type player = {
	gems : (string * string * string) list; 
	mutable scards : (string * string) list ;
	mutable sheet : bytes list;
	mutable info : bytes list}

type deck = {mutable cards : (string * string) list}
type mcard = {mutable card : (string * string * string)}

type status = {
	mutable turn : int;
	humplyrs : int;
	nplyrs : int;
	ai_lvl : int }

exception Quit of unit;;
