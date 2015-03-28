let parse entree =
        let ic = open_in entree in
        let (r,c,a,l,v,b,t,rs,cs) = Scanf.fscanf ic "%d %d %d %d %d %d %d %d %d " (fun i j k l m n o p q -> (i,j,k,l,m,n,o,p,q)) in
        let cibles = ref [] and mat = Array.init r (fun i -> Array.init c (fun j -> Array.init (a+1) (fun k -> (0,0)))) in
        for i = 1 to l do
                cibles := (Scanf.fscanf ic "%d %d " (fun x y -> (x,y)))::!cibles;
        done;
        for z=1 to a do
                for x=0 to r-1 do
                        for y=0 to c-1 do
                                mat.(x).(y).(z) <- Scanf.fscanf ic "%d %d " (fun x_a y_a -> (x_a,y_a));
                        done;
                done;
        done;
        (r,c,a,l,v,b,t,rs,cs,!cibles,mat);;

let (r,c,a,l,v,b,t,rs,cs,cibles,mat) = parse "final_round.in";;

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

let voisins (x,y,z) =
  let li = ref [] in
  let bouger d =
    let z' = z+d in
    let (u,v) = mat.(x).(y).(z') in
    let x' = x+u
    and y' = (y+v+c) mod c in
    if 0 <= x' && x' < r then
      li := (d, (x',y',z')) :: !li
  in
  (* rester à la même altitude *)
  if z >= 1 then
    bouger 0;
  (* monter *)
  if z < a then
    bouger 1;
  (* descendre *)
  if z >= 2 then
    bouger ~-1;
  !li

let mat_case_valide_bal bal cibles =
  let z = nb_valide (List.filter (fun t -> t != (-1,-1)) (List.map (fun (x,y,z) -> (x,y)) (Array.to_list bal))) in
  nb_valide (List.filter (fun (x,y) -> z.(x).(y) = 0) cibles)



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

let parcours_largeur x0 y0 x y dx dy =
  let t = Array.make_matrix r c (Array.make a []) in
  for i = 0 to r-1 do
    for j = 0 to c - 1 do
      t.(i).(j) <- (Array.make a [])
    done
  done;
  let f = Queue.create () in
  let k = ref 0 in
  Queue.add [(1,(x0,y0,1))] f;
  try
    while (!k) < 387420489 do
      incr k;
      let l = Queue.pop f in
      let v = voisins (snd (List.hd(l))) in
      List.iter (fun ((del,(i,j,k))) ->
	if abs(x-i) <= dx && abs(y-j) <= dy then raise (Trouve ((del,(i,j,k))::l))
	else
	    Queue.push ((del,(i,j,k))::l) f;) v;
    done;[]
  with |Trouve t -> t

let print_val r c  t sortie =
        let out = open_out sortie in
        for i = 0 to r - 1 do
                for j = 0 to c - 1 do
		  if t.(i).(j) = 0 then
		    Printf.fprintf out "_"
		  else if t.(i).(j) < 10 then
                    Printf.fprintf out "%d" t.(i).(j)
		  else
		    Printf.fprintf out "X";
                done;
                Printf.fprintf out "\n"
        done;;



let chemin x1 y1 x2 y2 dx dy = List.rev (parcours_largeur x1 y1 x2 y2 dx dy)

let chemin_australie x = chemin 19 100 19 260 10 0

(*let cibles = []
let () =
  print_val r c (mat_case_valide_bal (Array.make 1 (0,0,0)) cibles) "theo.out";;*)

