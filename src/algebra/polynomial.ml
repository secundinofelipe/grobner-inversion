(* polynomial.ml *)

type t = Term.t list

(* Ordena os termos, junta monômios iguais e remove coeficientes nulos. *)
let normalize (p : t) : t =
  let sorted =
    List.sort (fun t1 t2 -> Monomial.compare t2.monom t1.monom) p
  in

  let rec group acc current_list =
    match current_list with
    | [] -> List.rev acc
    | term :: rest ->
        match acc with
        | [] ->
            if term.coef.num = 0 then group acc rest
            else group [term] rest
        | last :: tail ->
            if Monomial.compare last.monom term.monom = 0 then
              (* Soma os coeficientes dos termos semelhantes *)
              let sum_coef = Rational.add last.coef term.coef in
              if sum_coef.num = 0 then
                group tail rest
              else
                group ({ Term.coef = sum_coef; monom = last.monom } :: tail) rest
            else if term.coef.num = 0 then
              group acc rest
            else
              group (term :: acc) rest
  in
  group [] sorted

(* Soma dois polinômios *)
let add (p1 : t) (p2 : t) : t =
  normalize (p1 @ p2)

(* Multiplica dois polinômios *)
let mul (p1 : t) (p2 : t) : t =
  let raw_terms =
    List.concat_map (fun t1 ->
      List.map (fun t2 -> Term.mul t1 t2) p2
    ) p1
  in
  normalize raw_terms

(* Mantém os termos ordenados *)
let sort (p : t) : t =
  normalize p

let to_string (p : t) =
  if p = [] then "0"
  else
    let terms_strs = List.map Term.to_string p in
    String.concat " + " terms_strs