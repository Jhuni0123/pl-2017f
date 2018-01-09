type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro
and name = string

let checkMetro (m: metro) : bool =
  let rec reduceMetro (m: metro): name list =
    match m with
    | STATION n -> [n]
    | AREA (n, m') -> List.filter (fun s -> s <> n) (reduceMetro m')
    | CONNECT (m1, m2) -> (reduceMetro m1) @ (reduceMetro m2)
  in reduceMetro m = []
