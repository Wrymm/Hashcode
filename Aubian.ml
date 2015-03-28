let parse entree =
	let ic = open_in entree in
	let (r,c,a,l,v,b,t,rs,cs) = Scanf.fscanf ic "%d %d %d %d %d %d %d %d %d " (fun i j k l m n o p q -> (i,j,k,l,m,n,o,p,q)) in
	let cibles = ref [] and mat = Array.init r (fun i -> Array.init c (fun j -> Array.init (a+1) (fun k -> (0,0)))) in
	for i = 1 to l do
		cibles := (Scanf.fscanf ic "%d %d " (fun x y -> (x,y)))::!cibles;
	done;
	for i=0 to r-1 do
		for j=0 to c-1 do
			for k=0 to a do
				mat.(i).(j).(k) <- Scanf.fscanf ic "%d %d " (fun x y -> (x,y));
			done;
		done;
	done;
	(r,c,a,l,v,b,t,rs,cs,!cibles,mat);;

let (r,c,a,l,v,b,t,rs,cs,cibles,mat) = parse "/home/guilaub/Documents/GitHub/mini_test.in";;
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

let mat_case_valide =
	let t = Array.make_matrix r c false in
	let rec aux = function
		|[] -> ()
		|(x,y)::q -> t.(x).(y) <- true; aux q
	in
aux cibles;
t;;


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

let apartir_de tab tours_restants =
	let resultat = ref [] in
		for i=1 to tours_restants do
			let resultat_tour = ref [] in
			for j=b-1 downto 0 do
				Printf.printf "42 %!\n";
				let mat_potent = mat_case_valide_val tab in
				Printf.printf "43 %!\n";
				if tab.(j) = (-1,-1,-1)
					then resultat_tour := 0::!resultat_tour
					else begin
					      Printf.printf "44 %!\n";
					     let vois = voisins (tab.(j)) in
					      Printf.printf "45 %!\n";
					     let (suiv,emplacement,_) = (max_l mat_potent vois) in
						tab.(j) <- emplacement;
						resultat_tour := suiv :: !resultat_tour;
					     end;
			done;
			resultat := !resultat_tour::!resultat;
		done;
	List.rev !resultat;;

apartir_de (Array.make b (rs,cs,0)) t;;
