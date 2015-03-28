exception OutBound

let valid_case t x y w h r s=
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


      
let tabpos t x y r s=
  let z = Array.make_matrix 13 13 false in
  for i = 1 to 12 do
    for j = 1 to 12 do
      if i*j >= 3 && i*j <= 12 then
	begin
	  if valid_case t x y i j r s then z.(i).(j) <- true;
	end
    done
  done;
  z
    
(*let () = let t = Array.make_matrix 3 5 false in
t.(1).(1) <- true; t.(1).(2) <- true; t.(1).(3) <- true;
let k = tabpos t 0 0 3 5 in
for i = 1 to 12 do
  for j = 1 to 12 do
    if k.(i).(j) then Printf.printf "Matrice %dx%d\n" i j;
  done
done;*)


