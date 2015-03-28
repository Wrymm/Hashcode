
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
	    if i*i +j*j <= 49 && (x+i) >= 0 && (x+i) < r then
	      t.(x+i).((y+j+c) mod c) <- t.(x+i).((y+j+c) mod c) + 1;
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
  
 
exception Trouve of ((int * (int*int*int)) list)
    
let parcours_largeur mat x0 y0 x y =
  let t = Array.make_matrix r c (Array.make a []) in
  for i = 0 to r-1 do
    for j = 0 to c - 1 do
      t.(i).(j) <- (Array.make a [])
    done
  done;
  let f = Queue.create () in
  Queue.add [(1,(x0,y0,1))] f;
  try
    while true do
      let l = Queue.pop f in
      let v = voisins (List.hd(l)) in
      List.iter (fun ((del,(i,j,k))) ->
	if (i,j) = (x,y) then raise (Trouve ((del,(i,j,k))::l))
	else
	    Queue.push ((del,(i,j,k))::l) f;) v;
    done;[]
  with |Trouve t -> t
 

let nb_valide l =
  let t = Array.make_matrix r c 0 in
  let rec aux = function
    |[] -> ()
    |(x,y)::q ->
	for i = -7 to 7 do
	  for j = -7 to 7 do
	    if i*i +j*j <= 49 && (x+i) >= 0 && (x+i) < r then
	      t.(x+i).((y+j+c) mod c) <- t.(x+i).((y+j+c) mod c) + 1;
	  done
	done;
	aux q
  in
  aux l;
  t;;

let print_val r c  t sortie =
        let out = open_out sortie in
        for i = 0 to r - 1 do
                for j = 0 to c - 1 do
		  if t.(i).(j) < 10 then
                    Printf.fprintf out "%d" t.(i).(j)
		  else
		    Printf.fprintf out "X";
                done;
                Printf.fprintf out "\n"
        done;;

let () = 
  print_val r c (nb_valide cibles) sortie;;
