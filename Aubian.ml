let parse entree =
	let ic = open_in entree in
	let (r,c,a,l,v,b,t,rs,cs) = Scanf.fscanf ic "%d %d %d %d %d %d %d %d %d " (fun i j k l m n o p q -> (i,j,k,l,m,n,o,p,q)) in
	let cibles = ref [] and mat = Array.init a (fun i -> Array.init r (fun j -> Array.init c (fun k -> (0,0)))) in
	for i = 1 to l do
		cibles := (Scanf.fscanf ic "%d %d " (fun x y -> (x,y)))::!cibles;
	done;
	for i=0 to a-1 do
		for j=0 to r-1 do
			for k=0 to r do
				mat.(i).(j).(k) <- Scanf.fscanf ic "%d %d " (fun x y -> (x,y));
			done;
		done;
	done;
	(r,c,a,l,v,b,t,rs,cs,!cibles,mat);;

(* let (r,c,a,l,v,b,t,rs,cs,cibles,mat) = parse "/home/guilaub/Documents/GitHub/final_round.in";; *)
(*
Nous avons besoin de:
	- Une fonction "mat_case_valide_val tab" qui étant donné le tableau des emplacements des différents ballons, et la liste des cibles, renvoie la matrice des interet

*)
(*
let apartir_de tab tours_restants =
	let tab = Array.make b (rs,cs,0) and resultat = ref [] in
		for i=1 to tours_restants do
			let resultat_tour = ref [] in
			for j=0 to b-1 do
				let (x,y) = ref
*)
