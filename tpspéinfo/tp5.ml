let rd_couple () = Random.float 1000., Random.float 1000. ;;

let rd_nuage n =
  let t= Array.make n (0.,0.) in
  for i = 0 to n-1 do
    t.(i) <- rd_couple ()
  done ;
  t
;;

let tab1 = [|(137.952822280455479, 579.517066764289893);
  (745.24288848465, 673.134497490137164);
  (466.113061566003125, 188.103956342865786);
  (944.098271788395095, 410.935431876780797);
  (186.89530977266773, 283.014495971758549);
  (136.342566575285474, 698.315452710874752);
  (345.004922664389085, 939.068336631267471);
  (891.680623462061476, 287.663161181994212)|]
;;

#load "graphics.cma" ;;
open Graphics ;;
open_graph " 1000x1000" ;;

let round x =
  (* arrondi un flottant a l'entier le plus proche *)
  if x -. (floor x) < 0.5 then int_of_float (floor x) else int_of_float (floor x) + 1
;;

let trace_nuage nuage =
  for i=0 to Array.length nuage - 1 do
    let x,y=nuage.(i) in
    fill_circle (round x) (round y) 3
  done ;;

let coloration p q =
  let x1, y1 = p and x2, y2 = q in
  set_color red ;
  fill_circle (round x1) (round y1) 5 ;
  set_color blue ;
  fill_circle (round x2) (round y2) 5 ;
  set_color black ;
;; 

let tab_int = [|2;50;13;8;2;12;48;16;10;46;81;21;30|] ;;

let tab1x = [|(136.342566575285474, 698.315452710874752);
    (137.952822280455479, 579.517066764289893);
    (186.89530977266773, 283.014495971758549);
    (345.004922664389085, 939.068336631267471);
    (466.113061566003125, 188.103956342865786);
    (745.24288848465, 673.134497490137164);
    (891.680623462061476, 287.663161181994212);
    (944.098271788395095, 410.935431876780797)|]
;;

let tab1y =  [|(466.113061566003125, 188.103956342865786);
    (186.89530977266773, 283.014495971758549);
    (891.680623462061476, 287.663161181994212);
    (944.098271788395095, 410.935431876780797);
    (137.952822280455479, 579.517066764289893);
    (745.24288848465, 673.134497490137164);
    (136.342566575285474, 698.315452710874752);
    (345.004922664389085, 939.068336631267471)|]
;;

let tb1 =
  [|(466.113061566003125, 188.103956342865786);
    (345.004922664389085, 939.068336631267471)|]
;;


let distance a b = sqrt (((fst b)-.(fst a))*.((fst b)-.(fst a)) +. ((snd b)-.(snd a))*.((snd b)-.(snd a)));;
 
let plus_proche_naif t = 
  let n = (Array.length t) in
  let min = ref (t.(0), t.(1)) in
  let imin = ref (distance t.(0) t.(1)) in
  for i = 0 to (n-1) do
    for j = 0 to (n-1) do 
      if i<>j && (distance t.(i) t.(j)) < !imin then 
        min := (t.(i), t.(j))
    done ; 
  done ; 
  !min 
;;

let fission tableau = let n = (Array.length tableau) in (Array.sub tableau 0 (n/2)) , (Array.sub tableau (n/2)  (n-(n/2)));;

let fusion t1 t2 func = 
  let n1 = (Array.length t1) and n2 = (Array.length t2) in
  let i1 = ref 0 and i2 = ref 0 in
  let t = Array.make (n1+n2) 0 in
  for i = 0 to (n1+n2 -1) do 
    if !i2 = n2 || !i1 < n1 && (func t1.(!i1) t2.(!i2)) then
      (t.(i) <- t1.(!i1) ;
      i1 := !i1 + 1 )
    else (t.(i) <- t2.(!i2) ; 
    i2 := !i2 + 1 )
  done ; t 
;;


let rec tri_fusion t func = match t with
    | [||] -> t
    | [|x|] -> t
    | t -> let (a, b) = (fission t) in (fusion (tri_fusion a func) (tri_fusion b func) func)
;;


