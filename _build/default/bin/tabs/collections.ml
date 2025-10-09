open Js_of_ocaml
open Js_of_ocaml.Dom_html
open Site

let doc = Dom_html.document

let get_el id coerce =
  Js.Opt.get (doc##getElementById (Js.string id)) (fun () -> assert false)
  |> coerce
  |> fun x -> Js.Opt.get x (fun () -> assert false)

let weighted_table = get_el "weighted-table" Dom_html.CoerceTo.table

let weighted_checkbox =
  get_el "weighted-weighted-checkbox" Dom_html.CoerceTo.input

let weighted_addrow = get_el "weighted-addrow" Dom_html.CoerceTo.button
let weighted_deleterow = get_el "weighted-deleterow" Dom_html.CoerceTo.button
let weighted_clear = get_el "weighted-clear" Dom_html.CoerceTo.button

let tbody =
  Js.Opt.get (weighted_table##.tBodies##item 0) (fun () -> assert false)

(* --- Helpers --- *)

let count_rows () = tbody##.rows##.length

let create_input ?(cls = "") ?(value = "") () =
  let inp =
    Js.Opt.get
      (Dom_html.CoerceTo.input (doc##createElement (Js.string "input")))
      (fun () -> assert false)
  in
  inp##.className := Js.string cls;
  inp##.value := Js.string value;
  inp##setAttribute (Js.string "type") (Js.string "text");
  Js.Unsafe.coerce inp

let recalc_weights () =
  if Js.to_bool weighted_checkbox##.checked then
    let n = count_rows () in
    if n > 0 then
      let w = 1.0 /. float_of_int n in
      for i = 0 to n - 1 do
        let row = Js.Opt.get (tbody##.rows##item i) (fun () -> assert false) in
        match
          Js.Opt.to_option (row##querySelector (Js.string ".weight-input"))
        with
        | Some input ->
            (Js.Unsafe.coerce input)##.value
            := Js.string (Printf.sprintf "%.3f" w)
        | None -> ()
      done

let add_row _ =
  let row = doc##createElement (Js.string "tr") in

  (* Item input cell *)
  let item_cell = doc##createElement (Js.string "td") in
  let item_input = create_input ~cls:"item-input" ~value:"" () in
  Dom.appendChild item_cell item_input;
  Dom.appendChild row item_cell;

  (* Optional weight input cell *)
  if Js.to_bool weighted_checkbox##.checked then (
    let weight_cell = doc##createElement (Js.string "td") in
    let weight_input = create_input ~cls:"weight-input" ~value:"0.0" () in
    Dom.appendChild weight_cell weight_input;
    Dom.appendChild row weight_cell);

  Dom.appendChild tbody row;
  recalc_weights ()

let delete_row _ =
  if count_rows () > 0 then (
    let last = Js.Opt.get tbody##.lastChild (fun () -> assert false) in
    Dom.removeChild tbody last;
    recalc_weights ())

let clear_table _ =
  while Js.Opt.test tbody##.firstChild do
    let child = Js.Opt.get tbody##.firstChild (fun () -> assert false) in
    Dom.removeChild tbody child
  done;
  recalc_weights ()

let toggle_weighted _ =
  let weighted = Js.to_bool weighted_checkbox##.checked in
  let header =
    Js.Opt.get
      (doc##getElementById (Js.string "weighted-table-weight-header"))
      (fun () -> assert false)
  in
  if weighted then (
    (* Show header *)
    header##.style##.display := Js.string "table-cell";

    (* Add weight inputs to existing rows *)
    for i = 0 to count_rows () - 1 do
      let row = Js.Opt.get (tbody##.rows##item i) (fun () -> assert false) in
      let weight_cell =
        Js.Opt.get
          (row##querySelector (Js.string ".weight-input"))
          (fun _ -> assert false)
      in
      weight_cell##setAttribute (Js.string "style") (Js.string "")
      (* let weight_cell = doc##createElement (Js.string "td") in
      let weight_input = create_input ~cls:"weight-input" ~value:"0.0" () in *)
      (* Dom.appendChild weight_cell weight_input;
      Dom.appendChild row weight_cell *)
    done;

    recalc_weights ())
  else (
    (* Hide header *)
    header##.style##.display := Js.string "none";

    (* Remove all weight cells *)
    let weights =
      Dom.list_of_nodeList (tbody##querySelectorAll (Js.string ".weight-input"))
    in
    List.iter
      (fun (inp : element Js.t) ->
        inp##setAttribute (Js.string "style") (Js.string "display: none"))
      weights)

let setup () =
  bind_click "weighted-addrow" add_row;
  bind_click "weighted-deleterow" delete_row;
  bind_click "weighted-clear" clear_table;
  bind_click "weighted-weighted-checkbox" toggle_weighted;

  (* Weighted Select *)
  bind_click "weighted-select" (fun () ->
      (* Gather all item rows *)
      let rows =
        List.init (count_rows ()) (fun i ->
            Js.Opt.get (tbody##.rows##item i) (fun () -> assert false))
      in
      let items = ref [] in
      let weights = ref [] in

      List.iter
        (fun (row : tableRowElement Js.t) ->
          match
            Js.Opt.to_option (row##querySelector (Js.string ".item-input"))
          with
          | Some item_inp ->
              let item_inp =
                Js.Opt.get (Dom_html.CoerceTo.input item_inp) (fun _ ->
                    assert false)
              in
              let item_val = Js.to_string item_inp##.value in
              if String.trim item_val <> "" then (
                items := item_val :: !items;
                if Js.to_bool weighted_checkbox##.checked then
                  match
                    Js.Opt.to_option
                      (row##querySelector (Js.string ".weight-input"))
                  with
                  | Some weight_inp ->
                      let weight_inp =
                        Js.Opt.get (Dom_html.CoerceTo.input weight_inp)
                          (fun _ -> assert false)
                      in
                      let w_str = Js.to_string weight_inp##.value in
                      let w = try float_of_string w_str with _ -> 1.0 in
                      weights := w :: !weights
                  | None -> weights := 1.0 :: !weights
                else weights := 1.0 :: !weights)
          | None -> ())
        rows;

      let items = List.rev !items in
      let weights = List.rev !weights in

      match items with
      | [] -> error "No items to select from"
      | _ -> (
          let choice =
            if Js.to_bool weighted_checkbox##.checked then
              Rng.Collections.choose ~weights items
            else Rng.Collections.choose items
          in
          match choice with
          | None -> error "Selection failed"
          | Some x -> append_text "Weighted Select" x))
