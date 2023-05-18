open Mk

let zero = ctr "Zero" []
let succ a = ctr "Succ" [ a ]

let rec nat = function
  | 0 -> zero
  | n when n > 0 -> succ @@ nat @@ (n - 1)
  | _ -> failwith "Only not-negative nambers may be convert to Peano numbers"

let rec add a b c =
  conde
    [
      a === zero &&& (b === c);
      fresh (fun a0 ->
          fresh (fun c0 -> a === succ a0 &&& (c === succ c0) &&& add a0 b c0));
    ]

let rec _add1 a b c =
  conde
    [
      a === zero &&& (b === c);
      fresh (fun a0 ->
          fresh (fun c0 ->
              _add1 a0 b c0 &&& (a === succ a0) &&& (c === succ c0)));
    ]

let rec _add2 a b c =
  conde
    [
      a === zero &&& (b === c);
      fresh (fun a0 ->
          fresh (fun c0 ->
              delay (fun () ->
                  _add2 a0 b c0 &&& (a === succ a0) &&& (c === succ c0))));
    ]

let _ =
  Printf.printf "Query: add x 1 3";
  print_states @@ run (-1) (fresh @@ fun x -> add x (nat 1) (nat 3));

  Printf.printf "\n\n\n";

  Printf.printf "Query: add x y 3";
  print_states
  @@ run (-1)
       ( fresh @@ fun x ->
         fresh @@ fun y -> add x y (nat 3) )
