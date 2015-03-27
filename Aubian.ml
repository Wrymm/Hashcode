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

let (r,c,h,s,mat_pizza) = parse "/home/guilaub/Documents/GitHub/test.in";;

(*
let test (i,j) a b mat_pizza mat_utilise =
	let nb_pizza = ref 0 in
	let flag = ref true in
	for k=i to i+a-1 do
		for l=j to j+b-1 do
			if (k >= r) || () (mat_utilise.(k).(l)
*)
