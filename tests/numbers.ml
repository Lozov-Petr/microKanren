open Mk

let zero   = ctr "Zero" []
let succ a = ctr "Succ" [a]

let rec add a b c =
  ((a === zero) &&& (b === c)) |||
  (fresh (fun a0 -> fresh (fun c0 ->
    (a === succ a0) &&&
    (c === succ c0) &&&
    (add a0 b c0)
    )))
    


let rec add1 a b c =
  ((a === zero) &&& (b === c)) |||
  (fresh (fun a0 -> fresh (fun c0 ->
    (add1 a0 b c0) &&&
    (a === succ a0) &&&
    (c === succ c0)
    )))



let rec add2 a b c =
  ((a === zero) &&& (b === c)) |||
  (fresh (fun a0 -> fresh (fun c0 -> delay (fun () ->
    (add2 a0 b c0) &&&
    (a === succ a0) &&&
    (c === succ c0)
    ))))
