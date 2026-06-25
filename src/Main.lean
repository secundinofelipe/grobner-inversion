import Mathlib
import Mathlib.Tactic


noncomputable section

open scoped BigOperators

universe u

variable {R : Type u} [CommRing R]

namespace VanDenEssen

/-- Dimensão do espaço afim. -/
variable (n : ℕ)

/--
Aplicação polinomial sobre `Rⁿ`.

Cada certificado gerado pelo algoritmo deverá fornecer uma definição
concreta para este tipo.
-/
abbrev PolyMap := (Fin n → R) → (Fin n → R)

/--
Uma aplicação é inversível quando existem aplicações `F` e `G`
tais que ambas as composições são a identidade.
-/
structure PolynomialInverse where
  F : PolyMap (R := R) n
  G : PolyMap (R := R) n
  left_inverse :
    ∀ y : Fin n → R, F (G y) = y
  right_inverse :
    ∀ x : Fin n → R, G (F x) = x

end VanDenEssen