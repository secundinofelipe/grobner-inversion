(* term.ml *)

type t = {
  coef : Rational.t;
  monom : Monomial.t;
}

let make coef monom = { coef; monom }

(* Multiplica dois termos *)
let mul t1 t2 =
  {
    coef = Rational.mul t1.coef t2.coef;
    monom = Monomial.mul t1.monom t2.monom;
  }

let to_string t =
  let c_str = Rational.to_string t.coef in
  let m_str = Monomial.to_string t.monom in
  if c_str = "1" && m_str <> "1" then m_str
  else if c_str = "-1" && m_str <> "1" then "-" ^ m_str
  else if m_str = "1" then c_str
  else c_str ^ m_str