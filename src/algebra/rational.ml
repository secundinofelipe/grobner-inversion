(* rational.ml *)

type t = { num : int; den : int }

let rec gcd a b =
  if b = 0 then abs a else gcd b (a mod b)

(* Cria um racional já simplificado *)
let make num den =
  if den = 0 then failwith "Divisao por zero"
  else
    let g = gcd num den in
    let n = num / g in
    let d = den / g in
    if d < 0 then { num = -n; den = -d } else { num = n; den = d }

let zero = make 0 1
let one = make 1 1

let add r1 r2 =
  make (r1.num * r2.den + r2.num * r1.den) (r1.den * r2.den)

let sub r1 r2 =
  make (r1.num * r2.den - r2.num * r1.den) (r1.den * r2.den)

let mul r1 r2 =
  make (r1.num * r2.num) (r1.den * r2.den)

let div r1 r2 =
  make (r1.num * r2.den) (r1.den * r2.num)

let to_string r =
  if r.den = 1 then string_of_int r.num
  else string_of_int r.num ^ "/" ^ string_of_int r.den