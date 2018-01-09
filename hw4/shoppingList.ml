type require =
  id * (cond list)
and cond =
  | Items of gift list
  | Same of id
  | Common of cond * cond
  | Except of cond * gift list
and gift =
  int
and id = A | B | C | D | E

let shoppingList (reqs: require list): (id * gift list) list =
  let (>>) f g x = g (f x) in
  let contain l x = List.mem x l in
  let sort = List.sort_uniq compare in
  let id_list = [A; B; C; D; E] in
  let rec iter n f x =
    match n with
    | n when n < 0 -> failwith "iter: should be n > 0"
    | 0 -> x
    | n -> iter (n-1) f (f x)
  in
  let rec iter_id id_list f x =
    match id_list with
    | [] -> x
    | h::t -> iter_id t f (f h x)
  in
  let rec get_id ans x =
    match ans with
    | [] -> []
    | (x', l)::ans' when x = x' -> l
    | h::ans' -> get_id ans' x
  in
  let rec set_id ans x l =
    match ans with
    | [] -> failwith "Invalid ans"
    | (x', l')::ans' when x' = x -> (x, l)::ans'
    | h::ans' -> h::(set_id ans' x l)
  in

  let rec eval res con =
    match con with
    | Items gl -> gl
    | Same x -> get_id res x
    | Common (c1, c2) -> List.filter (contain (eval res c1)) (eval res c2)
    | Except (c, gl) -> List.filter (contain gl >> not) (eval res c)
  in
  let augment x res =
    get_id reqs x |> List.map (eval res) |> List.concat |> sort |> set_id res x
  in
  let propagate res =
    iter_id id_list augment res
  in
  id_list |> List.map (fun x -> x, []) |> iter (List.length id_list) propagate

