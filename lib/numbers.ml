module Numbers = struct
  open Int64

  type int = Int64.t

  let ( + ) = Int64.add
  let ( - ) = Int64.sub
  let ( * ) = Int64.mul
  let ( / ) = Int64.div
  let ( % ) = Int64.rem
  let string_of_int = Int64.to_string

  (** [randint ?signed ?min ?max ()] returns a random 64-bit integer.
      - If [signed] is true (default), the default range is [min_int, max_int].
      - Otherwise, the default range is [0, max_int].
      - [min] and [max] override these defaults. *)
  let randint ?(signed = true) ?(min : int option) ?(max : int option) () :
      (int, string) result =
    let min = Option.value min ~default:(if signed then min_int / 2L else 0L)
    and max =
      Option.value max ~default:(if signed then max_int / 2L else max_int)
    in
    if max < min then Error "randint: max < min"
    else Ok (min + Random.int64 (max - min))

  (** [randfloat ?min ?max ()] returns a float in \[min, max\). *)
  let randfloat ?(min : float option) ?(max : float option) () :
      (float, string) result =
    let min = Option.value min ~default:(-1e6) in
    let max = Option.value max ~default:1e6 in
    if Float.is_nan min || Float.is_nan max then Error "randfloat: NaN input"
    else if max < min then Error "randfloat: max < min"
    else
      let range = max -. min in
      if Float.is_infinite range || range <= 0. then
        Error "randfloat: invalid range"
      else Ok (min +. Random.float range)
end
