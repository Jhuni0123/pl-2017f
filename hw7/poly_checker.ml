(*
 * SNU 4190.310 Programming Languages 2017 Fall
 * Type Checker Skeleton
 *)

open M

type var = string

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TVarIBSL of var
  | TVarIBS of var
  (* Modify, or add more if needed *)

type typ_scheme =
  | SimpleTyp of typ
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list

let count = ref 0

let new_var () =
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 =
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v | TVarIBSL v | TVarIBS v -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' =
    match t' with
    | TVar x'
    | TVarIBSL x'
    | TVarIBS x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let make_subst_var : var -> var -> subst = fun x y ->
  let rec subs t' =
    match t' with
    | TVar x' -> if x = x' then TVar y else t'
    | TVarIBSL x' -> if x = x' then TVarIBSL y else t'
    | TVarIBS x' -> if x = x' then TVarIBS y else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst_var alpha beta @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

let subst_genvar : typ_scheme -> typ = fun tyscm ->
  match tyscm with
  | SimpleTyp t -> t
  | GenTyp (alphas, t) ->
      let betas = List.map (fun _ -> new_var ()) alphas in
      let s' =
        List.fold_left2
          (fun acc_subst alpha beta -> make_subst_var alpha beta @@ acc_subst)
          empty_subst alphas betas
      in
      s' t


let rec include_var t x =
  match t with
  | TVar a
  | TVarIBSL a
  | TVarIBS a -> if a = x then true else false
  | TPair (t1, t2)
  | TFun (t1, t2) -> include_var t1 x || include_var t2 x
  | TLoc t -> include_var t x
  | TInt
  | TBool
  | TString -> false

let rec unify : typ * typ -> subst = fun (t, t') ->
  match t, t' with
  | (t, t') when t = t' -> empty_subst
  | (TVar a, t)
  | (t, TVar a) ->
      if include_var t a then raise (M.TypeError "Fail to unify")
      else make_subst a t
  | (TVarIBSL a, t)
  | (t, TVarIBSL a) ->
      if include_var t a then raise (M.TypeError "Fail to unify")
      else begin
        match t with
        | TInt | TBool | TString | TLoc _ | TVarIBS _ | TVarIBSL _ -> make_subst a t
        | _ -> raise (M.TypeError "Fail to unify")
      end
  | (TVarIBS a, t)
  | (t, TVarIBS a) -> begin
      match t with
      | TInt | TBool | TString | TVarIBS _ -> make_subst a t
      | _ -> raise (M.TypeError "Fail to unify")
      end
  | (TPair (t1, t2), TPair (t1', t2'))
  | (TFun (t1, t2), TFun (t1', t2')) ->
      let s = unify (t1, t1') in
      let s' = unify (s t2, s t2') in
      s' @@ s
  | (TLoc t, TLoc t') -> unify (t, t')
  | _ -> raise (M.TypeError "Fail to unify")

let rec expansive : M.exp -> bool = fun e ->
  match e with
  | M.CONST _ -> false
  | M.VAR _ -> false
  | M.FN _ -> false
  | M.APP _ -> true
  | M.LET (M.VAL (x, e1), e2) -> expansive e1 || expansive e2
  | M.LET (M.REC (f, x, e), e2) -> expansive e2
  | _ -> true
  (*| M.IF (e1, e2, e3) -> expansive e1 || expansive e2 || expansive e3
  | M.BOP (_, e1, e2) -> expansive e1 || expansive e2
  | M.READ -> false
  | M.WRITE e -> expansive e
  | M.MALLOC _ -> true
  | M.ASSIGN _ -> true
  | M.BANG e -> expansive e
  | M.SEQ (e1, e2) -> expansive e1 || expansive e2
  | M.PAIR (e1, e2) -> expansive e1 || expansive e2
  | M.FST e -> expansive e
  | M.SND e -> expansive e*)

let rec infer_M : typ_env * M.exp * typ -> subst = fun (tyenv, exp, t) ->
  match exp with
  | M.CONST (M.S s) -> unify (t, TString)
  | M.CONST (M.N n) -> unify (t, TInt)
  | M.CONST (M.B b) -> unify (t, TBool)
  | M.VAR x -> unify (t, subst_genvar (List.assoc x tyenv))
  | M.FN (x, e) ->
      let b1 = new_var () in
      let b2 = new_var () in
      let s1 = unify (t, TFun (TVar b1, TVar b2)) in
      let tyenv' = subst_env s1 tyenv in
      let s2 = infer_M ((x, SimpleTyp (s1 (TVar b1)))::tyenv', e, s1 (TVar b2)) in
      s2 @@ s1
  | M.APP (e1, e2) ->
      let b = new_var () in
      let s1 = infer_M (tyenv, e1, TFun (TVar b, t)) in
      let s2 = infer_M (subst_env s1 tyenv, e2, s1 (TVar b)) in
      s2 @@ s1
  | M.LET (M.VAL (x, e1), e2) ->
      let b = new_var () in
      let s1 = infer_M (tyenv, e1, TVar b) in
      let tyenv' = subst_env s1 tyenv in
      let tx =
        if expansive e1 then SimpleTyp (s1 (TVar b))
        else generalize tyenv' (s1 (TVar b))
      in
      let s2 = infer_M ((x, tx)::tyenv', e2, s1 t) in
      s2 @@ s1
  | M.LET (M.REC (f, x, e), e2) ->
      let e1 = M.FN (x, e) in
      let b = new_var () in
      let s1 = infer_M ((f, SimpleTyp (TVar b))::tyenv, e1, TVar b) in
      let tyenv' = subst_env s1 tyenv in
      let s2 = infer_M ((f, generalize tyenv' (s1 (TVar b)))::tyenv', e2, s1 t) in
      s2 @@ s1
  | M.IF (e_cond, e_true, e_false) ->
      let s1 = infer_M (tyenv, e_cond, TBool) in
      let tyenv' = subst_env s1 tyenv in
      let t' = s1 t in
      let s2 = infer_M (tyenv', e_true, t') in
      let tyenv'' = subst_env s2 tyenv' in
      let t'' = s2 t' in
      let s3 = infer_M (tyenv'', e_false, t'') in
      s3 @@ s2 @@ s1
  | M.BOP (M.ADD, e1, e2)
  | M.BOP (M.SUB, e1, e2) ->
      let s1 = unify (t, TInt) in
      let tyenv' = subst_env s1 tyenv in
      let s2 = infer_M (tyenv', e1, TInt) in
      let tyenv'' = subst_env s2 tyenv' in
      let s3 = infer_M (tyenv'', e2, TInt) in
      s3 @@ s2 @@ s1
  | M.BOP (M.AND, e1, e2)
  | M.BOP (M.OR, e1, e2) ->
      let s1 = unify (t, TBool) in
      let tyenv' = subst_env s1 tyenv in
      let s2 = infer_M (tyenv', e1, TBool) in
      let tyenv'' = subst_env s2 tyenv' in
      let s3 = infer_M (tyenv'', e2, TBool) in
      s3 @@ s2 @@ s1
  | M.BOP (M.EQ, e1, e2) ->
      let b = new_var () in
      let s1 = unify (t, TBool) in
      let tyenv' = subst_env s1 tyenv in
      let s2 = infer_M (tyenv', e1, TVarIBSL b) in
      let tyenv'' = subst_env s2 tyenv' in
      let s3 = infer_M (tyenv'', e2, s2 (TVarIBSL b)) in
      s3 @@ s2 @@ s1
  | M.READ -> unify (t, TInt)
  | M.WRITE e ->
      let b = new_var () in
      let s1 = unify (t, TVarIBS b) in
      let tyenv' = subst_env s1 tyenv in
      let s2 = infer_M (tyenv', e, s1 t) in
      s2 @@ s1
  | M.MALLOC e ->
      let b = new_var () in
      let s1 = unify (t, TLoc (TVar b)) in
      let tyenv' = subst_env s1 tyenv in
      let s2 = infer_M (tyenv', e, s1 (TVar b)) in
      s2 @@ s1
  | M.ASSIGN (e1, e2) ->
      let s1 = infer_M (tyenv, e1, TLoc t) in
      let tyenv' = subst_env s1 tyenv in
      let t' = s1 t in
      let s2 = infer_M (tyenv', e2, t') in
      s2 @@ s1
  | M.BANG e -> infer_M (tyenv, e, TLoc t)
  | M.SEQ (e1, e2) ->
      let b = new_var () in
      let s1 = infer_M (tyenv, e1, TVar b) in
      let tyenv' = subst_env s1 tyenv in
      let t' = s1 t in
      let s2 = infer_M (tyenv', e2, t') in
      s2 @@ s1
  | M.PAIR (e1, e2) ->
      let b1 = new_var () in
      let b2 = new_var () in
      let s1 = unify (t, TPair (TVar b1, TVar b2)) in
      let tyenv' = subst_env s1 tyenv in
      let s2 = infer_M (tyenv', e1, s1 (TVar b1)) in
      let tyenv'' = subst_env s2 tyenv' in
      let s3 = infer_M (tyenv'', e2, (s2 @@ s1) (TVar b2)) in
      s3 @@ s2 @@ s1
  | M.FST e ->
      let b = new_var () in
      infer_M (tyenv, e, TPair (t, TVar b))
  | M.SND e ->
      let b = new_var () in
      infer_M (tyenv, e, TPair (TVar b, t))

let rec convert : typ -> M.typ = fun t ->
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (convert t1, convert t2)
  | TLoc t -> M.TyLoc (convert t)
  | _ -> raise (M.TypeError "Invalid result type")

let check : M.exp -> M.typ = fun e ->
  let x = new_var () in
  let s = infer_M ([], e, TVar x) in
  let t = s (TVar x) in
  convert t
