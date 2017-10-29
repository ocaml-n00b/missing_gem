(* #use "./common_def.ml";; *)
open Common_def


let chk_gems plyr srch =
	let g  = plyr.gems in
	match srch with  (* For Testing *)
	| (x, "", "") -> let n = List.length (List.filter (fun a -> let (i,_,_)=a in (i=x) ) g) in
		Printf.printf "There are %d %s cards\n" n x;
		plyr.info <- (plyr.info)@[Bytes.of_string ( (string_of_int n)^" "^x^" cards." )];
		print_newline () ; []
	| ("", y, "") -> let n = List.length (List.filter (fun a -> let (_,j,_)=a in (j=y) ) g) in
		Printf.printf "There are %d %s cards\n" n y;
		plyr.info <- (plyr.info)@[Bytes.of_string  ( (string_of_int n)^" "^y^" cards.")];
		print_newline () ; []
	| ("", "", z) -> let n = List.length (List.filter (fun a -> let (_,_,k)=a in (k=z) ) g) in
		Printf.printf "There are %d %s cards\n" n z;
		plyr.info <- (plyr.info)@[Bytes.of_string  ( (string_of_int n)^" "^z^" cards." )];
		print_newline () ; []
	| (x, y, "") -> let tmp = List.filter (fun a -> let (i,j,_)=a in (i=x && j=y) ) g in
		let tmpstr =  (  (string_of_int (List.length tmp))^" ["^x^"] ["^y^"] cards." ) in 
		print_endline tmpstr ;
		plyr.info <-( (plyr.info)@[Bytes.of_string tmpstr] );
		tmp
	| (x, "", z) -> let tmp = List.filter (fun a -> let (i,_,k)=a in (i=x && k=z) ) g in
		let tmpstr = ((string_of_int (List.length tmp))^" ["^z^"] ["^x^"] cards." ) in
		print_endline tmpstr ;
		plyr.info <-( (plyr.info)@[Bytes.of_string tmpstr] );
		tmp
	| ("", y, z) -> let tmp = List.filter (fun a -> let (_,j,k)=a in (j=y && k=z) ) g in
		let tmpstr = ((string_of_int (List.length tmp))^" ["^z^"] ["^y^"] cards.") in
		print_endline tmpstr ;
		plyr.info <-( (plyr.info)@[Bytes.of_string tmpstr] );
		tmp
	| _ -> [];;
	
(* Get Players Hands*)
let init_plyrs plyrl = let rec aux acc = function
  | [] -> acc
  | h::t -> let (x,y,z) = h in
    aux ([{gems=x; scards=y; sheet=z; info=[]}]@acc) t
in aux [] plyrl;;
