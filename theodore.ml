let r = 100;;
let c = 300;;
let a = 8


let nb_valide l =
  let t = Array.make_matrix r c 0 in
  let rec aux = function
    |[] -> ()
    |(x,y)::q ->
	for i = -7 to 7 do
	  for j = -7 to 7 do
	    if i*i +j*j <= 49 then
	      t.(((x+i)+c) mod c).(y+j) <- t.(((x+i)+c) mod c).(y+j) + 1;
	  done
	done;
	aux q
  in
  aux l;
  t;;


let mat_case_valide l =
  let t = Array.make_matrix r c false in
  let rec aux = function
    |[] -> ()
    |(x,y)::q -> 
	t.(x).(y) <- true;
	aux q
  in
  aux l;
  t;;

let mat_case_valide_bal bal cibles =
  let z = mat_case_valide (List.map (fun (x,y,z) -> (x,y)) (Array.to_list bal))in
  let l = ref [] in
  let rec aux = function
    |[] -> []
    |(x,y)::q -> 
	if (not z.(x).(y)) then
	  l := (x,y)::(!l);
	aux q
  in
  mat_case_valide (aux cibles)
  
