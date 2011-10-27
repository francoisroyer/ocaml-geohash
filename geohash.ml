(*
DESCRIPTION
Ocaml functions to encode and decode geohashes.

AUTHOR
francoisroyer (at) gmail (dot) com

REFERENCES
http://en.wikipedia.org/wiki/Geohash
http://pleac.sourceforge.net/pleac_ocaml/numbers.html

ISSUES

*)

open Map;;
open List;;
open String;;
open Char;;

module CharMap = Map.Make(Char);;

let codetable = [
  '0',0;'1',1;'2',2;'3',3;'4',4;'5',5;'6',6;'7',7;'8',8;'9',9;
  'b',10;'c',11;'d',12;'e',13;'f',14;'g',15;
  'h',16;'j',17;'k',18;'m',19;'n',20;'p',21;
  'q',22;'r',23;'s',24;'t',25;'u',26;'v',27;
  'w',28;'x',29;'y',30;'z',31 ]
;;

let int_of_bool = function
   true -> 1
 | false -> 0
;;

let int_of_bits b = 
  List.fold_right (fun x z -> 2 * z + int_of_bool x) b 0
;;


let codemap = List.fold_left
  (fun m (k,d) -> CharMap.add k d m  ) CharMap.empty
  codetable
;;

let decode_hash hash = 
  let f h = 
    CharMap.find h codemap
  in
  List.map f hash
;;
 
let rec print_charlist m = match m with
  | [] -> print_char '\n'
  | h::t -> print_char h; print_char ' '; print_charlist t
;;

let rec print_intlist m = match m with
  | [] -> print_char '\n'
  | h::t -> print_int h; print_char ' '; print_intlist t
;;

let rec print_strlist m = match m with
  | [] -> print_char '\n'
  | h::t -> print_string h; print_char ' '; print_strlist t
;;

let charlist_of_string s =
  let nmax = String.length s - 1 in
  let rec aux s n = 
    if n<nmax
    then
      [String.get s n] @ aux s (n+1)
    else
      [String.get s n]
  in aux s 0 
;;


let binStr_of_decInt i =
  let rec strip_bits i s =
    match i with
      0 -> s
    | _ -> strip_bits (i lsr 1) ((string_of_int (i land 0x01)) ^ s) in
  strip_bits i "";;

let binStr_of_decStr i =
  let rec strip_bits i s =
    match i with
      0 -> s
    | _ -> strip_bits (i lsr 1) ((string_of_int (i land 0x01)) ^ s) in
  strip_bits (int_of_string i) "";;

(*-----------------------------*)
let decInt_of_binStr s =
  int_of_string ("0b" ^ s);;

let decStr_of_binStr s =
  string_of_int (int_of_string ("0b" ^ s));;
(*-----------------------------*)

let binList_of_intList d = 
  let f b  = binStr_of_decInt b in
    List.map f d;;

let pad_binStr str n = 
  let sl = String.length str in 
    match sl==n with
      | true -> str
      | false -> String.make (n-sl) '0' ^ str
;;

let pad_binStrList str n = 
  let f s = pad_binStr s n in
    List.map f str
;;

let pad5_binStrList str = pad_binStrList str 5;;

let concat_strList strlist = 
  let f s1 s2 = s1^s2  in
    List.fold_left f "" strlist
;;

(*
let print_strlist strlist = 
  let concat_strList strlist = 
    let f s1 s2 = s1^s2  in
      List.fold_left f "" strlist
  in
    print_endline (concat_strList strlist)
;;
*)

(*##### Find loc #######*)

(* unzip makes two lists out of one *)
let unzip mainlist = 
  let next l = match l with
    | [] -> []
    | h::t -> t in
  let rec every_other acc list =
    match list with 
      | [] -> acc
      | h::t -> every_other (acc @ [h]) (next t)
  in
    (every_other [] mainlist, every_other [] (List.tl mainlist)) 
;;

(* Given a list of chars, recurse to final loc  *)
let rec get_loc binvals range = 
  let select b l h = 
    let m = 0.5 *. (l +. h) in 
      match b with 
	| '0' -> (l,m)
	| _ -> (m,h) in
  let (low,high) = range in
  let mid = 0.5 *. (low +. high) in
  match binvals with 
    | [] -> mid
    | h::t -> get_loc t (select h low high)
;;

let loc_of_geohash gh = 
  let cl = charlist_of_string gh in
  let d = decode_hash cl in
  let binchain = binList_of_intList d in
  let padded = pad5_binStrList binchain in
  let binstring = concat_strList padded in
  let (binlon,binlat) = unzip (charlist_of_string binstring) in
  (get_loc binlon (-180.0,180.0), get_loc binlat (-90.0,90.0) )
;;

let (_) = 
  print_endline "\n**** BEGIN test ocaml-geohash ****\n";;

  (* Verbose *)
  let gh = "ezs42";; (* Should give 10.407, 57.649 *)
  print_endline ("Decoding: "^gh);;
  print_endline "\nThis gives in base 32:";;
  let cl = charlist_of_string gh;;
  let d = decode_hash cl;;
  print_intlist d;;

  let binchain = binList_of_intList d;;
  let padded = pad5_binStrList binchain;;
  print_strlist padded;;

  (* Direct result *)
  let (lon,lat) = loc_of_geohash gh;;
  
  print_endline "\nDecoded location is:";
  print_endline (string_of_float lon);;
  print_endline (string_of_float lat);;

  print_endline "\n**** END ****\n";;

(*
Another hash to try (from the Wikipedia article) is 
  u4pruydqqvj -> (57.64911,10.40744)
*)
