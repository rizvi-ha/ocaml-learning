type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec contains lst x = 
        match lst with
          [] -> false
        | (h::t) -> if x = h then true else contains t x
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
        | (h::t, lst) -> if contains lst h then subset t lst else false;;

let equal_sets a b = if subset a b && subset b a then true else false;;

let rec set_union a b = 
        match a with
        | [] -> b
        | h::t -> if contains b h then set_union t b else h::set_union t b 
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

let rec all_members_follow predicate lst = 
        match lst with
        | [] -> true
        | h::t -> if predicate h then all_members_follow predicate t else false;;

let filter_blind_alleys g =
        let is_terminal terminable_symbols symbol =
                match symbol with
                | T _ -> true
                | N a -> contains terminable_symbols a
        in
        let rec build_terminable_symbols rules terminable_symbols =
                match rules with
                | (lhs, rhs)::t -> if all_members_follow (is_terminal terminable_symbols) rhs then build_terminable_symbols t (lhs::terminable_symbols)
                                        else build_terminable_symbols t terminable_symbols
                | [] -> terminable_symbols
        in
        let bts_wrapper (rules, terminable_symbols) =
                (rules, build_terminable_symbols rules terminable_symbols)
        in
        let second_elements_equal (a, b) (c, d) =
                equal_sets b d
        in
        let terminable_set = snd (computed_fixed_point (second_elements_equal) (bts_wrapper) ((snd g), []))
        in
        let rec remove_bad_rules rules =
                match rules with
                | [] -> []
                | (lhs, rhs)::t -> if all_members_follow (is_terminal terminable_set) rhs then (lhs, rhs)::(remove_bad_rules t)
                                         else remove_bad_rules t
        in
        match g with
        | expr, rules -> expr, remove_bad_rules rules;;
