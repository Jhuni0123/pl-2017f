(*
 * SNU 4190.310 Programming Languages
 * Homework "Rozetta" Skeleton
 * Dongkwon Lee (dklee@ropas.snu.ac.kr)
 *)

let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z  -> Sonata.Z z
  | Sm5.B b  -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

(*
 * K (: Continuation)
 * Linked list implementation
 *
 * struct entry {
 *   #proc,
 *   #next
 * }
 *
 * - Init -
 * #stack_top := Null
 *
 * @push :
 *   #stack_top := {
 *     #proc := new proc,
 *     #next := !#stack_top
 *   }
 *
 * @pop :
 *   retval: !(!#stack_top).#proc
 *   #stack_top := !(!#stack_top).#next
 *)

let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) ->
      Sonata.Fn (arg,
        (trans' (command @
          [
          (* !(!#stack_top).#proc *)
          Sm5.PUSH (Sm5.Id "#stack_top");
          Sm5.LOAD;
          Sm5.UNBOX "#proc";
          Sm5.LOAD;

          (* #stack_top := !(!#stack_top).#next *)
          Sm5.PUSH (Sm5.Id "#stack_top");
          Sm5.LOAD;
          Sm5.UNBOX "#next";
          Sm5.LOAD;

          Sm5.PUSH (Sm5.Id "#stack_top");
          Sm5.STORE;

          (* push temporary argument *)
          Sm5.PUSH (Sm5.Val Sm5.Unit);
          Sm5.PUSH (Sm5.Id "#tmp_arg");

          (* call *)
          Sm5.CALL]) (* <- Sm5.CALL is at the end *)
        )
      )

and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)
  | Sm5.JTR (c1, c2) :: cmds ->
      (* translate with `cmds`. c1 or c2 can has `CALL` command. *)
      [Sonata.JTR (trans' (c1 @ cmds), trans' (c2 @ cmds))]
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET :: cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT :: cmds -> Sonata.PUT :: (trans' cmds)
  | Sm5.CALL :: cmds ->
      (match cmds with
      | [] ->
          (* single Sonata.CALL when Sm5.CALL is at the end *)
          [Sonata.CALL]
      | _ ->
          [
          (* == push == *)
          (* #next := !#stack_top *)
          Sonata.PUSH (Sonata.Id "#stack_top");
          Sonata.LOAD;

          Sonata.MALLOC;
          Sonata.BIND "#next";
          Sonata.PUSH (Sonata.Id "#next");

          Sonata.STORE;
          Sonata.UNBIND;

          (* #proc := new proc *)
          Sonata.PUSH (Sonata.Fn ("#tmp_arg",
              (* unbind temporary argument *)
              [Sonata.UNBIND;
              Sonata.POP
              ] @ trans' cmds));

          Sonata.MALLOC;
          Sonata.BIND "#proc";
          Sonata.PUSH (Sonata.Id "#proc");

          Sonata.STORE;
          Sonata.UNBIND;

          (* #stack_top := {#proc, #next} *)
          Sonata.BOX 2;
          Sonata.PUSH (Sonata.Id "#stack_top");
          Sonata.STORE;

          (* call *)
          Sonata.CALL]
      )
  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds)
  | Sm5.EQ :: cmds -> Sonata.EQ :: (trans' cmds)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds)
  | [] -> []

let trans : Sm5.command -> Sonata.command = fun command ->
  [
  (* #stack_top := Null *)
  Sonata.MALLOC; Sonata.BIND "#stack_top";
  Sonata.PUSH (Sonata.Val Sonata.Unit);
  Sonata.PUSH (Sonata.Id "#stack_top");
  Sonata.STORE;

  (* memory location for temporary argument *)
  Sonata.MALLOC; Sonata.BIND "#tmp_arg";
  ] @ (trans' command)
