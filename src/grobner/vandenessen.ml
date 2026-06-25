(* van_den_essen.ml *)

open Polynomial
open Buchberger
open MonomialOrder

(* Monta o ideal I = <y1 - F1, ..., yn - Fn>.
   A ordem das variáveis é:
   [y1; ...; yn; x1; ...; xn] *)
let build_ideal (map : Polynomial.t list) n =
  List.mapi (fun i fi ->
    (* y_i - F_i *)
    let y_i = { Term.coef = Rational.one; monom = List.init (2 * n) (fun j -> if j = i then 1 else 0) } in
    let neg_fi = List.map (fun t ->
      { Term.coef = Rational.mul t.Term.coef (Rational.make (-1) 1); monom = t.Term.monom }
    ) fi in
    Polynomial.add [y_i] neg_fi
  ) map

(* Verifica se a base reduzida tem a forma
   x_i - G_i(y). Se tiver, retorna a inversa. *)
let decide_invertibility (reduced_basis : Polynomial.t list) n =
  (* Espera uma base com n polinômios *)
  if List.length reduced_basis <> n then None
  else
    let rec extract_inverses basis acc =
      match basis with
      | [] -> Some (List.rev acc)
      | p :: ps ->
          (* Procura um polinômio da forma x_i - G_i(y) *)
          let is_xi_minus_gi poly =
            let sorted = List.sort (fun t1 t2 -> MonomialOrder.lex t2.Term.monom t1.Term.monom) poly in
            match sorted with
            | [t1; t2] ->
                let is_xi t =
                  t.Term.coef = Rational.one &&
                  (let m = t.Term.monom in
                   List.nth m (n + (n - List.length acc - 1)) = 1)
                in
                if is_xi t1 then
                  Some (
                    List.map
                      (fun t ->
                        {
                          Term.coef =
                            Rational.mul t.Term.coef (Rational.make (-1) 1);
                          monom = t.monom;
                        })
                      [t2]
                  )
                else None
            | _ -> None
          in
          match is_xi_minus_gi p with
          | Some gi -> extract_inverses ps (gi :: acc)
          | None -> None
    in
    extract_inverses reduced_basis []

(* Calcula a inversa usando o critério de van den Essen *)
let compute_inverse (map : Polynomial.t list) n =
  let ideal = build_ideal map n in

  (* Base de Gröbner na ordem de van den Essen *)
  let gb = buchberger ideal MonomialOrder.van_den_essen in
  let reduced_gb = reduce_grobner_basis gb MonomialOrder.van_den_essen in

  match decide_invertibility reduced_gb n with
  | Some inverse -> inverse
  | None -> failwith "Aplicação não é invertível pelo critério."