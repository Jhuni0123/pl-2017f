(*
 * SNU 4190.310 Programming Languages
 * K-- to SM5 translator skeleton code
 * DongKwon Lee (dklee@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct

  let rec trans : K.program -> Sm5.command = function
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT e -> trans e @ [Sm5.NOT]
    | K.ASSIGN (x, e) -> trans e @ [Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
    | K.IF (e_cond, e_true, e_false) -> trans e_cond @ [Sm5.JTR (trans e_true, trans e_false)]
    | K.WHILE (e_cond, e_body) ->
        trans (K.LETF ("@while_func", "#whlie_var", K.IF (e_cond, K.SEQ (e_body, K.CALLV ("@while_func", K.UNIT)), K.UNIT), K.CALLV ("@while_func", K.UNIT)))
    | K.FOR (x, e1, e2, e_body) ->
        trans (K.LETV ("#for_start", e1, K.LETV ("#for_end", e2, K.WHILE (K.NOT (K.LESS (K.VAR "#for_end", K.VAR "#for_start")), K.SEQ (K.ASSIGN (x, K.VAR "#for_start"), K.SEQ (e_body, K.ASSIGN ("#for_start", K.ADD (K.VAR "#for_start", K.NUM 1))))))))
    | K.LETV (x, e1, e2) ->
        trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
        trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.LETF (f, x, e1, e2) ->
        [Sm5.PUSH (Sm5.Fn (x, [Sm5.BIND f] @ trans e1 @ [Sm5.UNBIND; Sm5.POP])); Sm5.BIND f] @
        trans e2 @ [Sm5.UNBIND; Sm5.POP]
    | K.CALLV (f, arg_exp) -> [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ trans arg_exp @ [Sm5.MALLOC; Sm5.CALL]
    | K.CALLR (f, arg_var) -> [Sm5.PUSH (Sm5.Id f); Sm5.PUSH (Sm5.Id f)] @ trans (K.VAR arg_var) @ [Sm5.PUSH (Sm5.Id arg_var); Sm5.CALL]
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE e -> trans e @ [Sm5.MALLOC; Sm5.BIND "#write_var"] @ [Sm5.PUSH (Sm5.Id "#write_var"); Sm5.STORE] @ trans (K.VAR "#write_var") @ [Sm5.PUT] @ trans (K.VAR "#write_var") @ [Sm5.UNBIND; Sm5.POP]

end
