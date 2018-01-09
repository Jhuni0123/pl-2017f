module type ZEXPR =
  sig
    exception Error of string
    type id = string
    type expr =
      | NUM of int
      | PLUS of expr * expr
      | MINUS of expr * expr
      | MULT of expr * expr
      | DIVIDE of expr * expr
      | MAX of expr list
      | VAR of id
      | LET of id * expr * expr

    type environment
    type value

    val emptyEnv: environment
    val eval: environment * expr -> value

    val print_value : value -> unit
  end

module Zexpr : ZEXPR =
  struct
    exception Error of string
    type id = string
    type expr =
      | NUM of int
      | PLUS of expr * expr
      | MINUS of expr * expr
      | MULT of expr * expr
      | DIVIDE of expr * expr
      | MAX of expr list
      | VAR of id
      | LET of id * expr * expr

    type environment = (id * expr) list
    type value = int

    let emptyEnv = []
    let rec eval (env, e) : value =
      match e with
      | NUM num -> num
      | PLUS (e1, e2) -> (eval (env, e1)) + (eval (env, e2))
      | MINUS (e1, e2) -> (eval (env, e1)) - (eval (env, e2))
      | MULT (e1, e2) -> (eval (env, e1)) * (eval (env, e2))
      | DIVIDE (e1, e2) -> (eval (env, e1)) / (eval (env, e2))
      | MAX es -> begin
        match es with
        | [] -> 0
        | [e1] -> eval (env, e1)
        | h::t -> max (eval (env,h)) (eval (env, MAX t))
        end
      | VAR i -> begin
        match env with
        | [] -> raise (Error "FreeVariable")
        | (i', e')::env' when i' = i -> eval (env', e')
        | (i', e')::env' -> eval (env', VAR i)
        end
      | LET (id', e1, e2) -> eval ((id', e1)::env, e2)

    let print_value = print_int
  end

