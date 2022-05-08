let hachage_entier w k = k mod w;;

let list_expo x n = 
  let t = Array.make n 1 in
  for i = 1 to (n-1) do 
    t.(i) <- t.(i-1)*x
  done ; t ;;


let hachage_chaine cle chaine = 
  let x = ref 0 in 
  let n = String.length chaine in 
  let t = list_expo 128 n in
  for i = 0 to (n-1) do 
    x := ((int_of_char chaine.[i])*t.(i) mod cle)+ !x
  done ; !x mod cle;;

type ('a, 'b) table_hachage = { hache: 'a -> int; donnees: ('a * 'b) list array };;

let creer_table h w = {
  hache = h ;
  donnees = Array.make w []
};;

let petit_exemple = {
  hache = hachage_entier 3;
  donnees =
    [|[(15, "truc"); (468, "ocaml"); (498, "confinement"); (144, "TP")];
      [(1, "machin"); (154, "coucou")]; [(185, "info"); (512, "MPSI")]|]
  }
;;


let recherche t k = 
  let rec aux q = match q with 
    | [] -> false
    | x::r when (fst x) = k -> true
    | x::r -> aux r 
in aux (t.donnees.(t.hache k))
;;

let element t k =
  let rec aux q = match q with 
    | [] -> raise Not_found
    | x::r when (fst x) = k -> snd x
    | x::r -> aux r 
in aux (t.donnees.(t.hache k))
;;

let ajout t k e = let i = (t.hache k) in 
  if (not (recherche t k)) then 
    t.donnees.(i) <- (k,e)::t.donnees.(i)
;;

#load "graphics.cma" ;;
open Graphics ;;
open_graph " 650x650" ;;

let distance_max = 30 ;;

let trace_point c f=
  let x,y=f*fst c, f*snd c in
  moveto x y ;
  fill_circle x y 4 ;;

let trace_cercle c f r=
  let x,y=f*fst c, f*snd c in
  moveto x y ;
  draw_circle x y (f*r) ;;

let max_tab t=
(* max des coordonnees *)
  let n=Array.length t and m=ref 0 in
  for i=0 to n-1 do
    m:= max (max !m (fst t.(i))) (snd t.(i))
  done ;
  !m
;;

let trace_nuage tab=
  set_color blue ;
  let f=600 / (max_tab tab) in
  for i = 0 to Array.length tab - 1 do
    trace_point tab.(i) f
  done ;;

let random_nuage n d=
  let t=Array.make n (0,0) in
  for i=0 to n-1 do
    t.(i) <- (Random.int d, Random.int d)
  done ;
  t
;;

let nuage = random_nuage 1000 300 ;;

let hachage d (x,y) = (x/distance_max) + (d/distance_max)*(y/distance_max);;

let table d = creer_table (hachage d) (d/distance_max)*(d/distance_max);;

let creation_hach nuage d = creer_table (hachage d) (Array.length table) ;;

