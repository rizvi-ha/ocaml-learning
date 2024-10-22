let rec contains x lst = 
        match lst with
          [] -> false
        | (h::t) -> if x = h then true else contains x t


let rec subset a b = 
        match (a, b) with
          ([], _) -> true
        | (_, []) -> false
        | (h::t, lst) -> if contains h lst then subset t lst else false


