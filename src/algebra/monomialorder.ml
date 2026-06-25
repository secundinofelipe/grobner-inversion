(* Ordens monomiais *)

module MonomialOrder = struct
  (* Um monômio é representado por um vetor de expoentes *)
  type t = int list

  (* Grau total do monômio *)
  let degree (m : t) : int =
    List.fold_left (+) 0 m

  (* Ordem lexicográfica *)
  let rec lex (m1 : t) (m2 : t) : int =
    match m1, m2 with
    | [], [] -> 0
    | x :: xs, y :: ys ->
        let cmp = Stdlib.compare x y in
        if cmp <> 0 then cmp else lex xs ys
    | _ -> failwith "Monômios com dimensões incompatíveis"

  (* Ordem lexicográfica graduada *)
  let grlex (m1 : t) (m2 : t) : int =
    let d1 = degree m1 in
    let d2 = degree m2 in
    let cmp_deg = Stdlib.compare d1 d2 in
    if cmp_deg <> 0 then cmp_deg
    else lex m1 m2

  (* Ordem lexicográfica reversa graduada *)
  let grevlex (m1 : t) (m2 : t) : int =
    let rec revlex l1 l2 =
      match l1, l2 with
      | [], [] -> 0
      | x :: xs, y :: ys ->
          let res = revlex xs ys in
          if res <> 0 then res
          else Stdlib.compare y x
      | _ -> failwith "Monômios com dimensões incompatíveis"
    in
    let d1 = degree m1 in
    let d2 = degree m2 in
    let cmp_deg = Stdlib.compare d1 d2 in
    if cmp_deg <> 0 then cmp_deg
    else revlex m1 m2

  (* Ordem usada no critério de van den Essen *)
  let van_den_essen (m1 : t) (m2 : t) : int =
    let rec lex_rev l1 l2 =
      match l1, l2 with
      | [], [] -> 0
      | x :: xs, y :: ys ->
          let cmp = Stdlib.compare x y in
          if cmp <> 0 then cmp else lex_rev xs ys
      | _ -> failwith "Monômios com dimensões incompatíveis"
    in
    lex_rev (List.rev m1) (List.rev m2)
end

(* Extração de termos líderes *)

type rational = { num : int; den : int }
type term = { coef : rational; monom : MonomialOrder.t }
type polynomial = term list

module PolynomialExtract = struct

  (* Ordena os termos pela ordem monomial *)
  let sort_descending
      ~(cmp : MonomialOrder.t -> MonomialOrder.t -> int)
      (p : polynomial) : polynomial =
    List.sort (fun t1 t2 -> cmp t2.monom t1.monom) p

  (* Termo líder *)
  let leading_term
      ~(cmp : MonomialOrder.t -> MonomialOrder.t -> int)
      (p : polynomial) : term =
    match sort_descending ~cmp p with
    | [] -> failwith "O polinômio nulo não possui termo líder."
    | t :: _ -> t

  (* Coeficiente líder *)
  let leading_coefficient
      ~(cmp : MonomialOrder.t -> MonomialOrder.t -> int)
      (p : polynomial) : rational =
    (leading_term ~cmp p).coef

  (* Monômio líder *)
  let leading_power_product
      ~(cmp : MonomialOrder.t -> MonomialOrder.t -> int)
      (p : polynomial) : MonomialOrder.t =
    (leading_term ~cmp p).monom

  let leading_monomial
      ~(cmp : MonomialOrder.t -> MonomialOrder.t -> int)
      (p : polynomial) : MonomialOrder.t =
    leading_power_product ~cmp p

end

(* Testes *)

module Tests = struct
  open MonomialOrder

  let run_tests () =
    (* Lex *)
    assert (lex [2; 0; 0] [1; 2; 0] > 0);
    assert (lex [1; 2; 0] [2; 0; 0] < 0);
    assert (lex [1; 1; 1] [1; 1; 1] = 0);

    (* GrLex *)
    assert (grlex [2; 0; 0] [1; 2; 0] < 0);
    assert (grlex [2; 1; 0] [1; 2; 0] > 0);

    (* GrevLex *)
    assert (grevlex [1; 1; 2] [1; 2; 1] < 0);
    assert (grevlex [1; 2; 1] [1; 1; 2] > 0);

    (* van den Essen *)
    let m_y2 = [0; 1; 0; 0] in
    let m_x1 = [0; 0; 1; 0] in
    assert (van_den_essen m_x1 m_y2 > 0);

    let m_x1_y2 = [0; 1; 1; 0] in
    let m_y1_x2 = [1; 0; 0; 1] in
    assert (van_den_essen m_y1_x2 m_x1_y2 > 0);

    (* Extração do termo líder *)
    let c1 = { num = 3; den = 1 } in
    let c2 = { num = -5; den = 1 } in

    let t1 = { coef = c1; monom = [2; 0] } in
    let t2 = { coef = c2; monom = [0; 1] } in
    let p = [t1; t2] in

    let l_term = PolynomialExtract.leading_term ~cmp:van_den_essen p in
    assert (l_term.coef.num = -5);

    let l_monom = PolynomialExtract.leading_monomial ~cmp:van_den_essen p in
    assert (l_monom = [0; 1]);

    print_endline "Todos os testes passaram."
end

let () = Tests.run_tests ()