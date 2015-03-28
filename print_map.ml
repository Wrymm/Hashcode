let make_mat r c l =
        let t = Array.make_matrix r c false in
        List.iter (fun (x,y) -> t.(x).(y) <- true) l;
        t;;

let print_entree r c rs cs l sortie =
        let t = make_mat r c l in
        let out = open_out sortie in
        for i = 0 to r - 1 do
                for j = 0 to c - 1 do
                        if i = rs && j = cs then
                                Printf.fprintf out "O"
                        else if t.(i).(j) then
                                Printf.fprintf out "1"
                        else
                                Printf.fprintf out "_"
                done;
                Printf.fprintf out "\n"
        done;;

print_entree r c rs cs (!l_m) "out.out2";;
