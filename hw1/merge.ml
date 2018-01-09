let merge(l1, l2) =
  let rec merge_tail l1 l2 res =
    match (l1, l2) with
    | (l, []) | ([], l) -> (List.rev res) @ l
    | (h1::t1, h2::t2) ->
        if h1 = h2 then
          merge_tail t1 t2 (h1::res)
        else if h1 < h2 then
          merge_tail l1 t2 (h2::res)
        else
          merge_tail t1 l2 (h1::res)
  in
  merge_tail l1 l2 []

(*
let rec merge (l1, l2) =
    match l1, l2 with
    | (l, []) | ([], l) -> l
    | (h1::t1, h2::t2) when h1 > h2 -> h1::(merge (t1, l2))
    | (h1::t1, h2::t2) -> h2::(merge (l1, t2))
*)
