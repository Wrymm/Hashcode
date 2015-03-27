let valid_case t x y w h=
  let k = ref 0 in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      if t.(x+i).(y+j) then incr k;
    done
  done
  (!k >= 3)
      
