type var = int
type name = string
type term = Ctr of name * term list | Var of var

let ctr n a = Ctr (n, a)
let var i = Var i

type subst = (var * term) list

(************************************************************)

let rec walk : term -> subst -> term =
 fun term subst ->
  match term with
  | Var v -> (
      match List.assoc_opt v subst with Some t -> walk t subst | None -> term)
  | _ -> term

let rec occurs_check : var -> term -> subst -> bool =
 fun x t s ->
  match walk t s with
  | Var y -> x = y
  | Ctr (_, a) ->
      List.fold_left (fun acc r -> acc || occurs_check x r s) false a

let extend : var -> term -> subst -> subst option =
 fun x t s -> if occurs_check x t s then None else Some ((x, t) :: s)

let rec unify : term -> term -> subst -> subst option =
 fun t1 t2 subst ->
  let unify_option t1 t2 = function None -> None | Some s -> unify t1 t2 s in
  match (walk t1 subst, walk t2 subst) with
  | Var i, Var j when i = j -> Some subst
  | Var i, t -> extend i t subst
  | t, Var i -> extend i t subst
  | Ctr (n, a), Ctr (m, b) when n = m ->
      List.fold_right2 unify_option a b (Some subst)
  | _, _ -> None

(************************************************************)

type state = subst * int
type stream = Nil | Cons of state * stream | Thunk of (unit -> stream)
type goal = state -> stream

let nil = Nil
let cons a b = Cons (a, b)
let single : state -> stream = Fun.flip cons nil
let from_fun : (unit -> stream) -> stream = fun f -> Thunk f
let force : stream -> stream = function Thunk f -> f () | s -> s
let success : goal = fun s -> single s
let failure : goal = Fun.const nil

(************************************************************)

let rec mplus : stream -> stream -> stream =
 fun xs ys ->
  match xs with
  | Nil -> force ys
  | Cons (x, xs) -> cons x @@ from_fun (fun () -> mplus (force ys) xs)
  | Thunk _ -> from_fun @@ fun () -> mplus (force ys) xs

let rec bind : stream -> goal -> stream =
 fun xs g ->
  match xs with
  | Nil -> nil
  | Cons (x, xs) -> mplus (g x) (from_fun @@ fun () -> bind (force xs) g)
  | Thunk f -> from_fun @@ fun () -> bind (f ()) g

(************************************************************)

let delay : (unit -> goal) -> goal = fun f st -> from_fun (fun () -> f () st)

let ( === ) : term -> term -> goal =
 fun t1 t2 (subst, fresher) ->
  match unify t1 t2 subst with None -> nil | Some s -> single (s, fresher)

let ( ||| ) : goal -> goal -> goal =
 fun g1 g2 state -> mplus (g1 state) (g2 state)

let conde : goal list -> goal = function
  | [] -> success
  | g :: gs -> List.fold_right ( ||| ) gs g

let ( &&& ) : goal -> goal -> goal = fun g1 g2 state -> bind (g1 state) g2

let fresh : (term -> goal) -> goal =
 fun f (subst, fresher) -> f (var fresher) (subst, fresher + 1)

let rec take : int -> stream -> state list =
 fun n stream ->
  if n = 0 then []
  else
    match stream with
    | Nil -> []
    | Cons (x, xs) -> x :: take (n - 1) xs
    | Thunk f -> take n @@ f ()

let run : int -> goal -> state list = fun n goal -> take n @@ goal ([], 0)

(************************************************************)

let rec term2string : term -> string = function
  | Var v -> Printf.sprintf "_.%d" v
  | Ctr (n, []) -> n
  | Ctr (n, args) ->
      let args = List.map term2string args in
      Printf.sprintf "%s (%s)" n @@ String.concat ", " args

let subst2string : subst -> string =
 fun subst ->
  let subst =
    List.map (fun (var, term) ->
        Printf.sprintf "_.%d <- %s" var @@ term2string term)
    @@ List.sort (fun (n1, _) (n2, _) -> compare n1 n2) subst
  in
  String.concat "\n" subst

let state2string : state -> string =
 fun (subst, counter) ->
  Printf.sprintf "Var counter: %d\nSubstitution: \n%s" counter
  @@ subst2string subst

let print_states = List.iter (fun s -> Printf.printf "\n%s\n" @@ state2string s)
