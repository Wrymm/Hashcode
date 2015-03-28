let parse entree =
	let ic = open_in entree in
	let (r,c,h,s) = Scanf.fscanf ic "%d %d %d %d " (fun i j k l -> (i,j,k,l)) in
	let mat = Array.make_matrix r c false in
	for i=0 to r-1 do
		let mot = Scanf.fscanf ic "%s " (fun k -> k) in
			for j=0 to c-1 do
				mat.(i).(j) <- (mot.[j] = 'H');
			done;
	done;
	(r,c,h,s,mat);;

let (r,c,h,s,mat_pizza) = parse v

let test (i,j) a b mat_pizza mat_utilise =
	let nb_jambon = ref 0 in
	let flag = ref true in
	for k=i to i+a-1 do
		for l=j to j+b-1 do
			if (k >= r) || (l >= c) || (mat_utilise.(k).(l))
				then flag := false
				else if mat_pizza.(k).(l)
					then incr nb_jambon;
		done;
	done;
	if (!nb_jambon >= h && !flag)
		then for k=i to i+a-1 do
			for l=j to j+b-1 do
				mat_utilise.(k).(l) <- true;
			done;
		     done;
	(!nb_jambon >= h && !flag);;

let rec gere (i,j) mat_pizza mat_utilise = function
	|(a,b)::q	-> if test (i,j) a b mat_pizza mat_utilise
				then (a,b)
				else gere (i,j) mat_pizza mat_utilise q
	| []		-> (-1,-1);;



let shuffle = List.sort (fun i j -> if Random.int 2 = 0 then -1 else 1);;

let rec vaut = function | [] -> 0 | (a,b,c,d)::q -> c*d + vaut q;;

let final mat_pizza valeurs =
	let mat = Array.make_matrix r c false and l = ref [] in
	for i=0 to r-1 do
		for j=0 to c-1 do
			let (a,b) = gere (i,j) mat_pizza mat valeurs in
			if a >= 0 && b >= 0
				then l := (i,j,a,b) :: !l;
		done;
	done;
	!l,mat;;


let maxi = ref 0 and l = ref [];;
while !maxi < 9000 do
	let l' = fst (final mat_pizza (List.sort (fun (a,b) (c,d) -> -compare (compare (a*b) (b*d),if Random.int 2 = 0 then -1 else 1) (0,0)) [4,3;3,4;6,2;2,6;12,1;1,12;11,1;1,11;1,10;10,1;5,2;2,5;9,1;1,9;3,3;8,1;
					  1,8;4,2;2,4;1,7;7,1;1,6;6,1;3,2;2,3;5,1;1,5;4,1;1,4;2,2;1,3;3,1])) in
	let a = (vaut l') in
		if !maxi < a
			then (Printf.printf "%d %! \n" !maxi;maxi := a;l := l');
done;;


let l' = fst (final mat_pizza  (List.sort (fun i j -> compare (fst i) (fst j)) [4,3;3,4;6,2;2,6;12,1;1,12;11,1;1,11;1,10;10,1;5,2;2,5;9,1;1,9;3,3;8,1;
					  1,8;4,2;2,4;1,7;7,1;1,6;6,1;3,2;2,3;5,1;1,5;4,1;1,4;2,2;1,3;3,1])) in
	let a = (vaut l') in
		if !maxi < a
			then (Printf.printf "%d %! \n" !maxi;maxi := a;l := l');;
let parse_f l sortie =
	let oc = open_out sortie in
	Printf.fprintf oc "%d\n" (List.length l);
		List.iter (fun (a,b,c,d) -> Printf.fprintf oc "%d %d %d %d\n" a b (a+c-1) (b+d-1)) l;
	flush oc;;

parse_f !l "/home/guilaub/Documents/GitHub/resultat_round.in";;

let parse_fin sortie =
	let oc = open_out sortie in
	let (l,_) = final mat_pizza in
		Printf.fprintf oc "%d\n" (List.length l);
		List.iter (fun (a,b,c,d) -> Printf.fprintf oc "%d %d %d %d\n" a b (a+c-1) (b+d-1)) l;
	flush oc,vaut l;;

parse_fin "/home/guilaub/Documents/GitHub/resultat_round.in";;
