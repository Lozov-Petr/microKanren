open List

(************************************************************)

type var  = int
type name = string

type term = Ctr of name * term list
          | Var of var

let ctr n a = Ctr (n, a)
let var i   = Var i


type subst = (var * term) list
type state = subst * int

type stream = Nil
            | Cons of state * stream
            | Thunk of (unit -> stream)

type goal = state -> stream

let nil      = Nil
let cons a b = Cons (a, b)
let single a = cons a nil

let from_fun f  = Thunk f

let force s =
  match s with
  | Thunk f -> f ()
  | _       -> s

(************************************************************)

(* val walk : term -> subst -> term *)
let rec walk term subst =
  match term with
  | Var v ->
    begin match assoc_opt v subst with
    | Some t -> walk t subst
    | None   -> term
    end
  | _ -> term


(* val occurs_check : var -> term -> subst -> bool *)
let rec occurs_check x t s =
  match walk t s with
  | Var y      -> x = y
  | Ctr (_, a) -> fold_left (fun acc r -> acc || occurs_check x r s) false a


(* val extend : var -> term -> subst -> subst option *)
let extend x t s =
  if occurs_check x t s
    then None
    else Some ((x, t) :: s)


(* val unify : term -> term -> subst -> subst option *)
let rec unify t1 t2 subst =
  let unify_option t1 t2 = function
    | None -> None
    | Some s -> unify t1 t2 s in
  match walk t1 subst, walk t2 subst with
  | Var i     , Var j      when i = j -> Some subst
  | Var i     , t                     -> extend i t subst
  | t         , Var i                 -> extend i t subst
  | Ctr (n, a), Ctr (m, b) when n = m -> fold_right2 unify_option a b (Some subst)
  | _         , _                     -> None

(************************************************************)

(* val mplus : stream -> stream -> stream *)
let rec mplus xs ys =
  match xs with
  | Nil          -> force ys
  | Cons (x, xs) -> cons x @@ from_fun (fun () -> mplus (force ys) xs)
  | Thunk _      -> from_fun @@ fun () -> mplus (force ys) xs


(* val bind : stream -> goal -> stream *)
let rec bind xs g =
  match xs with
  | Nil          -> nil
  | Cons (x, xs) -> mplus (g x) (from_fun @@ fun () -> bind (force xs) g)
  | Thunk f      -> from_fun @@ fun () -> bind (f ()) g

(************************************************************)

(* val delay : (unit -> goal) -> goal *)
let delay f st =
  from_fun (fun () -> f () st)

(* val (===) : term -> term -> goal *)
let (===) t1 t2 (subst, fresher) =
  match unify t1 t2 subst with
  | None   -> nil
  | Some s -> single (s, fresher)


(* val (|||) : goal -> goal -> goal *)
let (|||) g1 g2 state =
  mplus (g1 state) (g2 state)


(* val (&&&) : goal -> goal -> goal *)
let (&&&) g1 g2 state =
  bind (g1 state) g2

(* val fresh : (term -> goal) -> goal *)
let fresh f (subst, fresher) =
  f (var fresher) (subst, fresher + 1)

(* val take : goal -> goal -> goal *)
let rec take n stream =
  if n = 0 then [] else
  match stream with
    | Nil          -> []
    | Cons (x, xs) -> x :: take (n - 1) xs
    | Thunk f      -> take n @@ f ()
