let sigma (a, b, f) =
  let rec sigma_tail (a, b, f, sum) =
    if a > b then sum
    else sigma_tail (a+1, b, f, sum + (f a))
  in sigma_tail (a, b, f, 0)

(*
let rec sigma (a, b, f) =
  if a > b then 0
  else a + (sigma (a+1, b, f))
*)
