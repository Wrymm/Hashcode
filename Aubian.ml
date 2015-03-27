let parse entree =
	let ic = open_in entree in
	let (m,n,o,p) = Scanf.scanf "%d %d %d %d " (fun i j k l -> (i,j,k,l)) in
	let mat = Array.make_matrix m n false in
	for i=1 to m do
		let mot = Scanf.fscanf ic "%s " (fun k -> k) in
			for j=0 to n-1 do
				mat.(i).(j) <- (mot.[j] = 'H');
			done;
	done;
	(m,n,o,p,mat);;

let test (i,j) a b =
	let flag = ref true in
	fo
