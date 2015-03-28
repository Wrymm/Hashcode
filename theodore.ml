let r = 100;;
let c = 300;;


let nb_valide l =
  let t = Array.make_matrix r c 0 in
  let rec aux = function
    |[] -> ()
    |(x,y)::q ->
	for i = -7 to 7 do
	  for j = -7 to 7 do
	    if i*i +j*j <= 49 then
	      t.(x+i).(y+j) <- t.(x+i).(y+j) + 1;
	  done
	done;
	aux q
  in
  aux l;
  t;;
