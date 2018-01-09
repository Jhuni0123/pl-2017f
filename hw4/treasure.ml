type treasure =
  | StarBox
  | NameBox of string
type key =
  | Bar
  | Node of key * key
type map =
  | End of treasure
  | Branch of map * map
  | Guide of string * map

type ikey =
  | IBar
  | INode of ikey * ikey
  | IVar of int
and vkey =
  | VUnknown
  | VKey of ikey

exception IMPOSSIBLE

let getReady (m: map): key list =
  let tmap : (treasure, ikey) Hashtbl.t = Hashtbl.create 100 in
  let varmap : (int, vkey) Hashtbl.t = Hashtbl.create 100 in
  let count = ref 0 in
  let newvar n =
    count := !count+n;
    Hashtbl.add varmap !count VUnknown;
    (IVar !count)
  in
  let lookup_treasure t : ikey =
    try Hashtbl.find tmap t
    with Not_found ->
      let ik =
        match t with
        | StarBox -> IBar
        | NameBox name -> newvar 1
      in
      Hashtbl.add tmap t ik;
      ik
  in
  let rec contain_var n ik : bool =
    match ik with
    | IVar n' when n = n' -> true
    | IVar n' ->
        let vk = Hashtbl.find varmap n' in
        ( match vk with
        | VUnknown -> false
        | VKey ik' -> contain_var n ik'
        )
    | INode (ik1, ik2) -> contain_var n ik1 || contain_var n ik2
    | _ -> false
  in
  let rec eval ik : key =
    match ik with
    | IBar -> Bar
    | IVar n ->
        let vk = Hashtbl.find varmap n in
        ( match vk with
        | VUnknown -> Bar
        | VKey ik' -> eval ik'
        )
    | INode (ik1, ik2) -> Node (eval ik1, eval ik2)
  in
  let rec unify ik1 ik2 : ikey =
    match (ik1, ik2) with
    | (ik1, ik2) when ik1 = ik2 -> ik1
    | (IVar n, ik1) | (ik1, IVar n) ->
        let vk2 = Hashtbl.find varmap n in
        ( match vk2 with
        | VUnknown ->
            if contain_var n ik1 then raise IMPOSSIBLE
            else Hashtbl.replace varmap n (VKey ik1); ik1
        | VKey ik2 -> unify ik1 ik2
        )
    | (IBar, _) | (_, IBar) -> raise IMPOSSIBLE
    | (INode (ik11, ik12), INode (ik21, ik22)) ->
        INode (unify ik11 ik21, unify ik12 ik22)
  in
  let rec explore (m: map): ikey =
    match m with
    | End t -> lookup_treasure t
    | Branch (m1, m2) ->
        let a = explore m1 in
        let b = explore m2 in
        let res = unify a (INode (b, newvar 1)) in
        ( match res with
        | INode (a, b) -> b
        | _ -> raise IMPOSSIBLE
        )
    | Guide (name, m) ->
        let a = lookup_treasure (NameBox name) in
        let b = explore m in
        INode (a, b)
  in
  let _ = explore m in
  Hashtbl.fold (fun k v l -> (eval v)::l) tmap [] |> List.sort_uniq compare

