(* division.ml *)

open Polynomial
open Term
open MonomialOrder

(* Pseudodivisão multivariada *)
let pseudodivision
    ~(f : Polynomial.t)
    ~(divisors : Polynomial.t list)
    ~(ord : t -> t -> int) =
  let t_div = List.length divisors in

  let rec loop p quocientes resto =
    if p = [] then (quocientes, resto)
    else
      let lt_p = PolynomialExtract.leading_term ~cmp:ord p in

      (* Procura um divisor cujo termo líder divide o de p *)
      let rec find_divisor i =
        if i >= t_div then None
        else
          let gi = List.nth divisors i in
          let lt_gi = PolynomialExtract.leading_term ~cmp:ord gi in

          if MonomialOrder.divides lt_gi.monom lt_p.monom then
            Some i
          else
            find_divisor (i + 1)
      in

      match find_divisor 0 with
      | Some i ->
          let gi = List.nth divisors i in
          let lt_gi = PolynomialExtract.leading_term ~cmp:ord gi in

          (* Termo usado na redução *)
          let m_coef = Rational.div lt_p.coef lt_gi.coef in
          let m_monom =
            List.map2 (-) lt_p.monom lt_gi.monom
          in
          let m = Term.make m_coef m_monom in

          (* Atualiza o quociente *)
          let new_quocientes =
            List.mapi
              (fun idx q ->
                if idx = i then Polynomial.add q [m] else q)
              quocientes
          in

          let m_poly = [m] in
          let subtraendo = Polynomial.mul m_poly gi in

          let p_novo =
            Polynomial.add p
              (List.map
                 (fun t ->
                   {
                     Term.coef =
                       Rational.mul t.coef (Rational.make (-1) 1);
                     monom = t.monom;
                   })
                 subtraendo)
          in

          loop p_novo new_quocientes resto

      | None ->
          (* O termo líder vai para o resto *)
          let novo_resto = Polynomial.add resto [lt_p] in
          let p_sem_lt = List.tl p in
          loop p_sem_lt quocientes novo_resto
  in

  loop f (List.init t_div (fun _ -> [])) []

(* Testes *)

module TestDivision = struct
  let run () =
    (* (x1² + x1x2) / x1 *)
    let open MonomialOrder in

    let f = [
      { coef = Rational.make 1 1; monom = [2; 0] };
      { coef = Rational.make 1 1; monom = [1; 1] }
    ] in

    let g = [[
      { coef = Rational.make 1 1; monom = [1; 0] }
    ]] in

    let (qs, r) =
      pseudodivision ~f ~divisors:g ~ord:lex
    in

    assert (r = []);

    let q1 = List.hd qs in
    assert (List.length q1 = 2);

    print_endline "Divisao simples passou."
end