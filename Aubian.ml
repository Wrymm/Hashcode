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

let (r,c,a,l,v,b,t,rs,cs,cibles,mat) = parse "final_round.in";;

let rec max_l mat_potent = function
	| []   -> (0,(-1,-1,-1),min_int)
	| (changement,(a,b,c))::q -> let (changement_reste,empl_reste,max_reste) = (max_l mat_potent q) and potent = mat_potent.(a).(b) in
				if potent > max_reste
					then (changement,(a,b,c),potent)
					else (changement_reste,empl_reste,max_reste);;

let nb_valide l =
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

let shuffle = List.sort (fun i j -> if Random.int 2 = 0 then 1 else -1);;

let iter_cercle f (x,y) =
  for i = -7 to 7 do
    for j = -7 to 7 do
      if i*i +j*j <= 49 && (x+i) >= 0 && (x+i) < r then
        f (x+i, (y+j+c) mod c)
    done;
  done;;

let mise_a_jour_nb_valide t (x,y as ballon) =
  iter_cercle (
    iter_cercle (fun (x'',y'') ->
      t.(x'').(y'') <- t.(x'').(y'') - 1
    )
  ) ballon;;




exception Trouve of ((int * (int*int*int)) list)

let parcours_largeur x0 y0 x y dx dy =
  let t = Array.make_matrix r c (Array.make a []) in
  for i = 0 to r-1 do
    for j = 0 to c - 1 do
      t.(i).(j) <- (Array.make a [])
    done
  done;
  let f = Queue.create () in
  let k = ref 0 in
  Queue.add [(1,(x0,y0,1))] f;
  try
    while (!k) < 387420489 do
      incr k;
      let l = Queue.pop f in
      let v = voisins (snd (List.hd(l))) in
      List.iter (fun ((del,(i,j,k))) ->
	if abs(x-i) <= dx && abs(y-j) <= dy then raise (Trouve ((del,(i,j,k))::l))
	else
	    Queue.push ((del,(i,j,k))::l) f;) v;
    done;[]
  with |Trouve t -> t

let chemin x1 y1 x2 y2 dx dy = List.rev (parcours_largeur x1 y1 x2 y2 dx dy);;

let chemin_australie = chemin rs cs 19 260 10 0;;

(*
let apartir_de tab tours_restants =
	let debut_amerique = ref [] and debut_australie = ref chemin_australie in
	let resultat = ref [] in
		for i=1 to tours_restants do
			let resultat_tour = ref [] and mat_potent = nb_valide cibles in
			for j=b-1 downto 0 do
				if j > 35 && (!debut_amerique != [])
					then (let (dep,coord) = List.hd !debut_amerique in
							tab.(j) <- coord;
							resultat_tour := dep::!resultat_tour;)
					else if j > 17 && (!debut_australie != [])
						then (let (dep,coord) = List.hd !debut_australie in
							tab.(j) <- coord;
							resultat_tour := dep::!resultat_tour;)
						else if tab.(j) = (-1,-1,-1)
							then resultat_tour := 0::!resultat_tour
							else begin
							    let vois = shuffle (voisins (tab.(j))) in
					     		    let (suiv,emplacement,_) = (max_l mat_potent vois) in
								tab.(j) <- emplacement;
								resultat_tour := suiv :: !resultat_tour;
								let (emp1,emp2,_) = emplacement in mise_a_jour_nb_valide mat_potent (emp1,emp2);
					    		     end;
				if !debut_amerique != []
					then debut_amerique := List.tl !debut_amerique;
				if !debut_australie != []
					then debut_australie := List.tl !debut_australie;
			done;
			resultat := !resultat_tour::!resultat;
			Printf.printf "%d %! \n" i;
		done;
	List.rev !resultat;;
*)

let apartir_de tab tours_restants =
	let debut_amerique = ref [] and debut_australie = ref chemin_australie in
	let resultat = ref [] in
		for i=1 to tours_restants do
			let resultat_tour = ref [] and mat_potent = nb_valide cibles in
			for j=b-1 downto 0 do
				if j > 35 && (!debut_amerique != [])
					then (let (dep,coord) = List.hd !debut_amerique in
							tab.(j) <- coord;
							resultat_tour := dep::!resultat_tour;)
					else if j > 17 && (!debut_australie != [])
						then (let (dep,coord) = List.hd !debut_australie in
							tab.(j) <- coord;
							resultat_tour := dep::!resultat_tour;)
						else if tab.(j) = (-1,-1,-1)
							then resultat_tour := 0::!resultat_tour
							else begin
							    let vois = shuffle (voisins (tab.(j))) in
					     		    let (suiv,emplacement,_) = (max_l mat_potent vois) in
								tab.(j) <- emplacement;
								resultat_tour := suiv :: !resultat_tour;
								let (emp1,emp2,_) = emplacement in mise_a_jour_nb_valide mat_potent (emp1,emp2);
					    		     end;
				if !debut_amerique != []
					then debut_amerique := List.tl !debut_amerique;
				if !debut_australie != []
					then debut_australie := List.tl !debut_australie;
			done;
			resultat := !resultat_tour::!resultat;
			Printf.printf "%d %! \n" i;
		done;
	List.rev !resultat;;


let resultat = apartir_de (Array.make b (rs,cs,0)) 400;;

let parse_fin sortie =
	let oc = open_out sortie in
		List.iter (fun i -> Printf.fprintf oc "%d" (List.hd i);List.iter (fun j -> Printf.fprintf oc " %d" j) (List.tl i); Printf.fprintf oc "\n") resultat; flush oc;;

parse_fin "final_resultat";;
