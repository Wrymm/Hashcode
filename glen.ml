let r = 5
let c = 20
let a = 8
let mat = Array.init r (fun x -> Array.make_matrix c a (1,1))


let voisins x y z =
  let li = ref [] in
  let bouger (u,v) =
    let x' = x+u
    and y' = (y+v+c) mod c in
    if 0 <= x' && x' < r then
      li := (x',y') :: !li
  in
  (* rester à la même altitude *)
  if z >= 1 then
    bouger mat.(x).(y).(z);
  (* monter *)
  if z < a then
    bouger mat.(x).(y).(z+1);
  (* descendre *)
  if z >= 2 then
    bouger mat.(x).(y).(z-1);
  !li
