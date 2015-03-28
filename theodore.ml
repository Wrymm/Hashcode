
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
	    if i*i +j*j <= 49 && (y+j) >= 0 && (y+j) < c then
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
  let z = mat_case_valide (List.filter (fun t -> t != (-1,-1)) (List.map (fun (x,y,z) -> (x,y)) (Array.to_list bal))) in
  let l = ref [] in
  let rec aux = function
    |[] -> []
    |(x,y)::q -> 
	if (not z.(x).(y)) then
	  l := (x,y)::(!l);
	aux q
  in
  mat_case_valide (aux cibles)
  
let voisins_tours x y z maximum = 
  let t = Array.make (maximum+1) [] in
  t.(0) <- [(x,y,z)];
  let rec aux = function
    |[] -> []
    |t::q -> (List.map (fun (_,t) -> t) (voisins t))@(aux q)
  in
  for i =1 to maximum do
    t.(i) <- (aux t.(i-1))
  done;
  t
  
      
