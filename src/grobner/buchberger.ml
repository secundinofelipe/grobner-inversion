(* buchberger.ml *)

open Polynomial
open Term
open Division

(* Gera todos os pares distintos da lista *)
let rec generate_pairs = function
  | [] -> []
  | f :: rest -> (List.map (fun g -> (f, g)) rest) @ generate_pairs rest

(* Calcula o S-polinômio de f e g *)
let s_polynomial f g ord =
  let lt_f = PolynomialExtract.leading_term ~cmp:ord f in
  let lt_g = PolynomialExtract.leading_term ~cmp:ord g in
  let lcm_m = MonomialOrder.lcm lt_f.monom lt_g.monom in

  let m1 = {
    coef = Rational.div (Rational.make 1 1) lt_f.coef;
    monom = List.map2 (-) lcm_m lt_f.monom
  } in
  let m2 = {
    coef = Rational.div (Rational.make 1 1) lt_g.monom
  } in (* Simplificado *)

  let term_f = {
    coef = Rational.div (Rational.make 1 1) lt_f.coef;
    monom = List.map2 (-) lcm_m lt_f.monom
  } in
  let term_g = {
    coef = Rational.div (Rational.make 1 1) lt_g.coef;
    monom = List.map2 (-) lcm_m lt_g.monom
  } in

  Polynomial.add
    (Polynomial.mul [term_f] f)
    (Polynomial.mul [{ coef = Rational.make (-1) 1; monom = term_g.monom }] g)

(* Algoritmo principal de Buchberger *)
let buchberger generators ord =
  let rec loop basis pairs =
    match pairs with
    | [] -> basis
    | (f, g) :: rest ->
        let _, r =
          pseudodivision ~f:(s_polynomial f g ord) ~divisors:basis ~ord
        in
        if r <> [] then
          (* Adiciona o novo polinômio e cria os pares correspondentes *)
          let new_pairs = List.map (fun b -> (r, b)) basis in
          loop (r :: basis) (rest @ new_pairs)
        else
          loop basis rest
  in
  loop generators (generate_pairs generators)

(* Deixa a base reduzida *)
let reduce_grobner_basis basis ord =
  (* Normaliza cada polinômio para ficar mônico *)
  let monic_basis =
    List.map
      (fun p ->
        let lc = PolynomialExtract.leading_coefficient ~cmp:ord p in
        List.map
          (fun t ->
            { coef = Rational.div t.coef lc; monom = t.monom })
          p)
      basis
  in

  (* Reduz cada polinômio pelos demais *)
  let rec inter_reduce current_basis =
    let rec aux acc = function
      | [] -> acc
      | p :: ps ->
          let others = acc @ ps in
          let _, r = pseudodivision ~f:p ~divisors:others ~ord in
          aux (r :: acc) ps
    in
    aux [] current_basis
  in

  List.filter (fun p -> p <> []) (inter_reduce monic_basis)