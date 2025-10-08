# RNG

See the site [here!](https://charles.systems/rng/)

This is a static site that performs pseudorandom number generation in a number of formats.
All functionality is implemented in OCaml and compiled to JavaScript using [js_of_ocaml](https://github.com/ocsigen/js_of_ocaml).

## Structure

Page layout and IO is all handled in [main.ml](./bin/main.ml), [site.ml](lib/site.ml), or [tabs](./bin/tabs/).
