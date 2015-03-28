
(*
let geo l h =
	let x = ref 0 in
	let y = ref 0 in
	let lst = ref [] in
	let s = ref 0 in
	while y + h < R do begin
		while 
	end
*)

let C = 60 in
let R = 180 in

let rec score lst = match lst with
	| [] -> 0
	| (r1,c1,r2,c2)::s -> (r2 - r1 + 1) * (c2 - c1 + 1) + score s in

let opti =
	let s = ref 0 in
	let l = ref [] in
	for i = 2 to 4 do
		for j = 3 to 6 do
			let x = ref 0 in
			let y = ref 0 in
			let cur_s = ref 0 in
			let cur_l = ref [] in
			while !y + R / j < R do
				while !x + C / i < C do
					let v = opti_loc x y (C / i) (R / j) in
					cur_s := !cur_s + score v;
					cur_l := v@!cur_l;
					x := !x + C / i
				done
			done;
			if !cur_s > !s then
			begin
				s := !cur_s;
				l := !cur_l
			end
		done
	done;
	!s
