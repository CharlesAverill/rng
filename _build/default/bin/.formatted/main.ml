open Js_of_ocaml
open Site

let download_console_log () =
  (* 3. Concatenate textContent of all divs *)
  let texts =
    let node_list = console_el##querySelectorAll (Js.string ".console-line") in
    let n = node_list##.length in
    let lines = ref [] in
    for i = 0 to n - 1 do
      let node = Js.Opt.get (node_list##item i) (fun _ -> assert false) in
      match Js.Opt.to_option (Dom_html.CoerceTo.element node) with
      | Some el ->
          let text =
            Js.Opt.to_option el##.textContent
            |> Option.value ~default:(Js.string "")
            |> Js.to_string
          in
          lines := !lines @ [ text ]
      | None -> ()
    done;
    String.concat "\n" !lines
  in

  (* 4. Create blob *)
  let blob = File.blob_from_string texts in

  (* 5. Create temporary <a> to download *)
  let url = Dom_html.window##.URL##createObjectURL blob in
  let a = Dom_html.createA Dom_html.document in
  a##.href := url;
  a##.download := Js.string "rng_console_log.txt";
  Dom.appendChild Dom_html.document##.body a;
  a##click;
  Dom.removeChild Dom_html.document##.body a

let () =
  Random.self_init ();

  Tabs.Numbers.setup ();

  bind_click "download-log" download_console_log;
  bind_click "clear-console" clear_console_lines;
  bind_click "toggle-params" (fun _ -> show_params := not !show_params);

  (* Distributions *)
  bind_click "gaussian" (fun () -> error "Gaussian RNG not implemented yet");

  bind_click "exponential" (fun () ->
      error "Exponential RNG not implemented yet");

  (* Weighted *)
  bind_click "weighted-select" (fun () ->
      match Rng.Collections.choose [ "a"; "b"; "c" ] with
      | None -> error "No choice made"
      | Some x -> append_text "Weighted Select" x)

(* Optional: clear console on entropy upload *)
(* let entropy_input = get_el "entropy" in
  ignore
    (Dom_html.addEventListener entropy_input Dom_html.Event.change
       (Dom_html.handler (fun _ ->
            clear_console_lines ();
            append_text "[Entropy]" "Entropy file uploaded";
            Js._false))
       Js._false) *)
