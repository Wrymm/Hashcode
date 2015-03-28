let r = 5
let c = 20
let a = 8
let mat = Array.init r (fun x -> Array.make_matrix c a (1,1))


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

let iter_cercle f (x,y) =
  for i = -7 to 7 do
    for j = -7 to 7 do
      if i*i +j*j <= 49 && (x+i) >= 0 && (x+i) < r then
        f (x+i, (y+j+c) mod c)
    done
  done

let mise_a_jour_nb_valide t (x,y as ballon) =
  iter_cercle (
    iter_cercle (fun (x'',y'') ->
      t.(x'').(y'') <- t.(x'').(y'') - 1
    )
  ) ballon
