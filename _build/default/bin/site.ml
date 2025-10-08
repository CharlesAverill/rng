open Js_of_ocaml
open Js_of_ocaml.Dom_html

let doc = Dom_html.document

(* --- Helpers --- *)

let idx = ref 0
let show_params = ref true

let get_el id =
  Js.Opt.get
    (doc##getElementById (Js.string id))
    (fun () -> failwith ("Element not found: " ^ id))

let console_el = get_el "console"
let download_btn = get_el "download-log"

let append_text identifier s =
  let div = createDiv doc in
  div##.className := Js.string "console-line";
  div##.textContent :=
    Js.some
      (Js.string
         (Printf.sprintf "[%d] %s%s" !idx
            (if !show_params then "[" ^ identifier ^ "] " else "")
            s));
  idx := !idx + 1;
  Dom.appendChild console_el div;
  (* Auto-scroll to bottom *)
  console_el##.scrollTop := Js.number_of_float (float console_el##.scrollHeight)

let error = append_text "Error"

let clear_console_lines () =
  (* Grab all console lines *)
  let lines = console_el##querySelectorAll (Js.string ".console-line") in
  let n = lines##.length - 1 in
  for i = n downto 0 do
    let node = Js.Opt.get (lines##item i) (fun () -> assert false) in
    Dom.removeChild console_el node
  done

let bind_click id handler =
  let el = get_el id in
  ignore
    (Dom_html.addEventListener el Dom_html.Event.click
       (Dom_html.handler (fun _ ->
            handler ();
            Js._false))
       Js._false)
