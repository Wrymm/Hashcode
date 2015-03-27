let parse entree =
	let ic = open_in entree in
	let (r,c,h,s) = Scanf.scanf "%d %d %d %d " (fun i j k l -> (i,j,k,l)) in
	let mat = Array.make_matrix r c false in
	for i=0 to r-1 do
		let mot = Scanf.fscanf ic "%s " (fun k -> k) in
			for j=0 to c-1 do
				mat.(i).(j) <- (mot.[j] = 'H');
			done;
	done;
	(r,c,h,s,mat);;

(*
let test (i,j) a b mat =
	let nb_pizza 
	let flag = ref true in
	for k=0 to a-1 do
		for l=0 to b-1 do
			flag := 
*)
