(* interface.ml *)

module Interface = struct
  open Polynomial
  open VanDenEssen
  open MonomialOrder

  (* Executa o algoritmo e mostra o resultado *)
  let calcula_inversa ~(ordem : (t -> t -> int)) (caso : unit -> Polynomial.t list) =
    let start_time = Sys.time () in

    print_endline "--------------------------------------------------";
    print_endline "Inversa pelo critério de van den Essen";
    print_endline "--------------------------------------------------";

    (* Casos de teste de Hubbers têm dimensão 4 *)
    let n = 4 in
    let f = caso () in

    let resultado =
      try
        compute_inverse ~map:f ~vars_n:n ~ord:ordem
      with _ ->
        None
    in

    let end_time = Sys.time () in
    let elapsed = end_time -. start_time in

    Printf.printf "Elapsed: %.6f segundos\n\n" elapsed;

    match resultado with
    | Some inv ->
        print_endline "Aplicação inversa:";
        print_endline "G = (";
        List.iteri
          (fun i p ->
            Printf.printf "  G_%d = %s%s\n"
              (i + 1)
              (Polynomial.to_string p)
              (if i < n - 1 then "," else ""))
          inv;
        print_endline ")"
    | None ->
        print_endline "Resultado: A aplicação não é invertível pelo critério.";

    print_endline "--------------------------------------------------";
end