let parse entree =
	let ic = open_in entree in
	let (r,c,a,l,v,b,t,rs,cs) = Scanf.fscanf ic "%d %d %d %d %d %d %d %d %d " (fun i j k l m n o p q -> (i,j,k,l,m,n,o,p,q)) in
	let cibles = ref [] and mat = Array.init r (fun i -> Array.init c (fun j -> Array.init (a+1) (fun k -> (0,0)))) in
	for i = 1 to l do
		cibles := (Scanf.fscanf ic "%d %d " (fun x y -> (x,y)))::!cibles;
	done;
	for z=1 to a do
		for x=0 to r-1 do
			for y=0 to c-1 do
				mat.(x).(y).(z) <- Scanf.fscanf ic "%d %d " (fun x_a y_a -> (x_a,y_a));
			done;
		done;
	done;
	(r,c,a,l,v,b,t,rs,cs,!cibles,mat);;

let (r,c,a,l,v,b,t,rs,cs,cibles,mat) = parse "/home/guilaub/Documents/GitHub/final_round.in";;
(*
Nous avons besoin de:
	- Une fonction "mat_case_valide_val tab" qui étant donné le tableau des emplacements des différents ballons, et la liste des cibles, renvoie la matrice des interet

*)

let rec max_l mat_potent = function
	| []   -> (0,(-1,-1,-1),min_int)
	| (changement,(a,b,c))::q -> let (changement_reste,empl_reste,max_reste) = (max_l mat_potent q) and potent = mat_potent.(a).(b) in
				if potent > max_reste
					then (changement,(a,b,c),potent)
					else (changement_reste,empl_reste,max_reste);;



let nb_valide l =
  try (
  let t = Array.make_matrix r c 0 in
  let rec aux = function
    |[] -> ()
    |(x,y)::q ->
	for i = -7 to 7 do
	  for j = -7 to 7 do
	    if i*i +j*j <= 49 && (x+i) >= 0 && (x+i) < r then
	      t.(x+i).((y+j+c) mod c) <- t.(x+i).((y+j+c) mod c) + 1;
	  done
	done;
	aux q
  in
  aux l;
  t)
   with Invalid_argument (_) -> failwith "nb_valide";;


let mat_case_valide l =
  try (
	let t = Array.make_matrix r c false in
  let rec aux = function
    |[] -> ()
    |(x,y)::q -> 
	t.(x).(y) <- true;
	aux q
  in
  aux l;
  t )
   with Invalid_argument (_) -> failwith "mat_case_valide";;

let mat_case_valide_bal bal =
  let z = mat_case_valide (List.filter (fun t -> t != (-1,-1)) (List.map (fun (x,y,z) -> (x,y)) (Array.to_list bal))) in
  let l = ref [] in
  let rec aux = function
    |[] -> []
    |(x,y)::q -> 
	if (not z.(x).(y)) then
	  l := (x,y)::(!l);
	aux q
  in
  nb_valide (aux cibles);;

let voisins (x,y,z) =
  let li = ref [] in
  let bouger d =
    let z' = z+d in
    let (u,v) = mat.(x).(y).(z') in
    let x' = x+u
    and y' = (y+v+c) mod c in
    if 0 <= x' && x' < r then
      li := (d, (x',y',z')) :: !li
  in
  (* rester à la même altitude *)
  if z >= 1 then
    bouger 0;
  (* monter *)
  if z < a then
    bouger 1;
  (* descendre *)
  if z >= 2 then
    bouger ~-1;
  !li;;


let shuffle = List.sort (fun i j -> if Random.int 2 = 0 then 1 else -1);;

let apartir_de tab tours_restants =
	let resultat = ref [] in
		for i=1 to tours_restants do
			let resultat_tour = ref [] in
			for j=b-1 downto 0 do
				let mat_potent = mat_case_valide_bal tab in
				if tab.(j) = (-1,-1,-1)
					then resultat_tour := 0::!resultat_tour
					else begin
					     let vois = shuffle (voisins (tab.(j))) in
					     let (suiv,emplacement,_) = (max_l mat_potent vois) in
						tab.(j) <- emplacement;
						resultat_tour := suiv :: !resultat_tour;
					     end;
				     
			done;
			resultat := !resultat_tour::!resultat;
		done;
	List.rev !resultat;;

apartir_de (Array.make b (rs,cs,0)) 400;;
