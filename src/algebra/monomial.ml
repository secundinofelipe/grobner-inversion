(* monomial.ml *)

(* Vetor de expoentes de um monômio *)
type t = int list

(* Ordem lexicográfica *)
let rec compare m1 m2 =
  match m1, m2 with
  | [], [] -> 0
  | x :: xs, y :: ys ->
      let cmp = Stdlib.compare x y in
      if cmp <> 0 then cmp else compare xs ys
  | _ -> failwith "Monômios com dimensões incompativeis"

(* Multiplica dois monômios *)
let mul m1 m2 =
  List.map2 (+) m1 m2

(* Verifica se m1 divide m2 *)
let divides m1 m2 =
  List.for_all2 (<=) m1 m2

(* Máximo divisor comum *)
let gcd m1 m2 =
  List.map2 min m1 m2

(* Mínimo múltiplo comum *)
let lcm m1 m2 =
  List.map2 max m1 m2

(* Grau total *)
let degree m =
  List.fold_left (+) 0 m

(* Converte para string *)
let to_string m =
  let terms =
    List.mapi (fun i exp ->
      if exp = 0 then ""
      else if exp = 1 then "x" ^ string_of_int (i + 1)
      else "x" ^ string_of_int (i + 1) ^ "^" ^ string_of_int exp
    ) m
  in
  let filtered = List.filter (fun s -> s <> "") terms in
  if filtered = [] then "1" else String.concat "" filtered