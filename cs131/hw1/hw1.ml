let rec contains x lst = 
        match lst with
          [] -> false
        | (h::t) -> if x = h then true else contains x t
;;

let reverse lst = 
        let rec aux lst acc = 
                match lst with
        | [] -> acc
        | h::t -> aux t (h::acc)
        in aux lst [];;

let rec subset a b = 
        match (a, b) with
          ([], _) -> true
        | (_, []) -> false
        | (h::t, lst) -> if contains h lst then subset t lst else false;;

let equal_sets a b = if subset a b && subset b a then true else false;;

let rec set_union a b = 
        match a with
        | [] -> b
        | h::t -> if contains h b then set_union t b else h::set_union t b 
;;

let set_all_union a =
        let rec combine lst acc = 
                match lst with 
                | [] -> acc
                | h::t -> combine t (set_union h acc)
        in combine a [];;

let rec computed_fixed_point eq f x = 
        if eq x (f x) then x else computed_fixed_point eq f (f x);;

let rec computed_periodic_point eq f p x = 
        let rec apply_n_times f n x = 
                match n with
                | 0 -> x
                | a -> f (apply_n_times f (n-1) x)
        in if eq x (apply_n_times f p x) then x else computed_periodic_point eq f p (f x);;

let whileseq s p x = 
        let rec helper s p x acc =
                match p x with
                | false -> acc
                | true -> helper s p (s x) (x::acc)
        in reverse (helper s p x []);;
