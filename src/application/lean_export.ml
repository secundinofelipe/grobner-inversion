(* lean_export.ml *)

module LeanExport = struct
  open Polynomial
  open Term

  (* Converte um polinômio para a sintaxe do Lean *)
  let poly_to_lean p =
    if p = [] then "0"
    else
      let term_to_str t =
        let coef = Rational.to_string t.coef in
        let vars = List.mapi (fun i exp ->
          if exp = 0 then ""
          else "x" ^ string_of_int (i + 1) ^
               (if exp > 1 then "^" ^ string_of_int exp else "")
        ) t.monom in
        let var_str = String.concat " * " (List.filter (fun s -> s <> "") vars) in
        if var_str = "" then coef
        else if coef = "1" then var_str
        else coef ^ " * " ^ var_str
      in
      String.concat " + " (List.map term_to_str p)

  let generate_file_content name f g =
    let n = List.length f in
    let header =
      "import Mathlib.Data.Polynomial.Basic\n\nopen Polynomial\n\n"
    in

    (* Define as aplicações F e G *)
    let f_def =
      "def F (x : Fin " ^ string_of_int n ^ " -> R) : (Fin " ^
      string_of_int n ^ " -> R) := \n  ![" ^
      String.concat ", " (List.map poly_to_lean f) ^ "]\n\n"
    in

    let g_def =
      "def G (y : Fin " ^ string_of_int n ^ " -> R) : (Fin " ^
      string_of_int n ^ " -> R) := \n  ![" ^
      String.concat ", " (List.map poly_to_lean g) ^ "]\n\n"
    in

    (* Provas das composições *)
    let composition =
      "theorem F_comp_G (y : Fin " ^ string_of_int n ^
      " -> R) : F (G y) = y := by\n  unfold F G\n  simp\n  decide\n\n\
theorem G_comp_F (x : Fin " ^ string_of_int n ^
      " -> R) : G (F x) = x := by\n  unfold F G\n  simp\n  decide\n"
    in

    header ^ f_def ^ g_def ^ composition

  let export_to_file filename f g =
    let oc = open_out filename in
    Printf.fprintf oc "%s" (generate_file_content filename f g);
    close_out oc;
    Printf.printf "Arquivo %s gerado com sucesso para Lean 4.\n" filename
end