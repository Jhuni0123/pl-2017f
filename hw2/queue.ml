module type Queue =
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end

module IntListQ =
  struct
    type element = int list
    type queue = element list * element list
    exception EMPTY_Q
    let emptyQ = ([], [])
    let enQ ((ql, qr), e) = (e::ql, qr)
    let deQ q =
      match q with
      | (ql, h::qr) -> (h, (ql,qr))
      | (ql, []) ->
          match List.rev ql with
          | [] -> raise EMPTY_Q
          | h::q -> (h, ([],q))
  end
