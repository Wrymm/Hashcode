exception OutBound

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

let (r,c,h,s,mat_pizza) = parse "test_round.in";;

let valid_case t x y w h=
  let k = ref 0 in
  try
    for i = 0 to w-1 do
      for j = 0 to h-1 do
        if (x+i) >= r || (y+j) >= s then raise OutBound;
        Printf.printf "zblah %d %d\n" i j;
        if t.(x+i).(y+j) then incr k;
      done
    done;
      (!k) >= 3
with |OutBound -> false

let tabpos t x y =
  let z = Array.make_matrix 13 13 false in
  for i = 1 to 12 do
    for j = 1 to 12 do
      if i*j >= 3 && i*j <= 12 then
        begin
          if valid_case t x y i j then z.(i).(j) <- true;
        end
    done
  done;
  z

let rec score lst = match lst with
	| [] -> 0
	| (r1,c1,r2,c2)::s -> (r2 - r1 + 1) * (c2 - c1 + 1) + score s;;

let valt = Array.make_matrix 180 60 true;;

let opti_loc x y a b =
	let rec aux i j =
		let w = ref [] in
		let z = tabpos mat_pizza i j in
		let xc = [1,2,3] in
		let yc = [12,6,4] in
		for k = 0 to 2 do
			if z.(xc.(k)).(yc.(k)) && valt.(xc).(yc) then
				for xa = x to x + xc.(k) - 1 do
					for ya = y to y + yc.(k) - 1 do
						valt.(xc).(yc) <- false
					done;
				done;
			let s = ref ((xc.(k) - x + 1) * (yc.(k) - y + 1)) in
			let l = ref [(x,y)] in
			for g = y to y + yc.(k) do
				l := (aux (x + xc.(k)) g)@(!l)
			done;
			for g = x to x + xc.(k) do
                                l := (aux g (y + yc.(k)))@(!l)
                        done;
			if score (!l) > score (!w) then w := !l;
			for xa = x to x + xc.(k) - 1 do
                                for ya = y to y + yc.(k) - 1 do
                                	valt.(xc).(yc) <- true
                        	done;
			done
		done;
		!w in
	aux x y;;
			
		

let opti =
	let s = ref 0 in
	let l = ref [] in
	for i = 2 to 4 do
		for j = 3 to 6 do
			let x = ref 0 in
			let y = ref 0 in
			let cur_s = ref 0 in
			let cur_l = ref [] in
			while !y + r / j <= r do
				x := 0;
				while !x + c / i <= c do
					let v = opti_loc x y (c / i) (r / j) in
					cur_s := !cur_s + score v;
					cur_l := v@(!cur_l);
					x := !x + c / i
				done;
				y := !y + r / j
			done;
			if !cur_s > !s then
			begin
				s := !cur_s;
				l := !cur_l
			end
		done
	done;
	!l;;

let _ =
	print_int (score opti);
	print_newline();;
