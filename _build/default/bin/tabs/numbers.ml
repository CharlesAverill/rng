open Js_of_ocaml
open Site
open Rng.Numbers

type randint_settings = {
  mutable signed : bool;
  mutable min : int64;
  mutable max : int64;
}

let randint_settings = { signed = false; min = 0L; max = Int64.max_int }

type randfloat_settings = { mutable min : float; mutable max : float }

let randfloat_settings = { min = -1e6; max = 1e6 }

let setup () =
  bind_click "randint" (fun () ->
      (* 1. Update settings from DOM *)
      let signed_cb = get_el "signed-checkbox" |> Js.Unsafe.coerce in
      randint_settings.signed <- signed_cb##.checked;

      let min_input = get_el "randint-min-number" |> Js.Unsafe.coerce in
      randint_settings.min <- Int64.of_string min_input##.value;

      let max_input = get_el "randint-max-number" |> Js.Unsafe.coerce in
      randint_settings.max <- Int64.of_string max_input##.value;

      (* validate settings *)
      if not randint_settings.signed then (
        randint_settings.min <- Int64.max 0L randint_settings.min;
        randint_settings.max <-
          Int64.max (Int64.add 1L randint_settings.min) randint_settings.max);
      (* update DOM with validated settings *)
      ( get_el "randint-min-number" |> Dom_html.CoerceTo.input |> fun x ->
        Js.Opt.get x (fun _ -> failwith "randint-min-number cast fail") )##.value
      := Js.string (Int64.to_string randint_settings.min);
      ( get_el "randint-max-number" |> Dom_html.CoerceTo.input |> fun x ->
        Js.Opt.get x (fun _ -> failwith "randint-max-number cast fail") )##.value
      := Js.string (Int64.to_string randint_settings.max);

      (* 2. Generate RNG *)
      match
        Numbers.randint ~signed:randint_settings.signed
          ~min:randint_settings.min ~max:randint_settings.max ()
      with
      | Error s -> error s
      | Ok n ->
          append_text
            (Printf.sprintf "int(%b, %Ld, %Ld)" randint_settings.signed
               randint_settings.min randint_settings.max)
            (Int64.to_string n));

  bind_click "randfloat" (fun () ->
      (* 1. Update settings from DOM *)
      let min_input = get_el "randfloat-min-number" |> Js.Unsafe.coerce in
      randfloat_settings.min <- Float.of_string min_input##.value;

      let max_input = get_el "randfloat-max-number" |> Js.Unsafe.coerce in
      randfloat_settings.max <- Float.of_string max_input##.value;

      (* 2. Generate RNG *)
      match
        Numbers.randfloat ~min:randfloat_settings.min
          ~max:randfloat_settings.max ()
      with
      | Error s -> error s
      | Ok n ->
          append_text
            (Printf.sprintf "float(%f, %f)" randfloat_settings.min
               randfloat_settings.max)
            (Float.to_string n))
