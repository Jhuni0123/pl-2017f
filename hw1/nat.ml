type nat =
  | ZERO
  | SUCC of nat

let natadd (n1, n2) =
  let rec natadd' (n1, n2) =
    match n1 with
    | ZERO -> n2
    | SUCC n' -> natadd' (n', SUCC n2)
  in
  if n1 < n2 then natadd' (n1, n2)
  else natadd' (n2, n1)

let natmul (n1, n2) =
  let rec natmul' (n1, n2) res =
    match n1 with
    | ZERO -> res
    | SUCC n' -> natmul' (n', n2) (natadd (n2, res))
  in
  if n1 < n2 then natmul' (n1, n2) ZERO
  else natmul' (n2, n1) ZERO

(*
let rec natadd(n1, n2) =
  match n1 with
  | ZERO -> n2
  | SUCC n' -> natadd (n', SUCC n2)

let rec natmul(n1, n2) =
  match n1 with
  | ZERO -> ZERO
  | SUCC n' -> natadd (n2, natmul (n', n2))
*)
