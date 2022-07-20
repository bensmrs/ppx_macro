(** This module tests the PPX with Alcotest *)

open Alcotest

let%macro foo = 2
let%macro bar = 2 * bar
let%macro baz = ( * ) 2

(** A check for simple macro reuse *)
let test_use () =
  check int "can use macros" 2 [%use foo]

(** A check for macro bindings *)
let test_binding () =
  check int "uses proper bindings" 6 (let bar = 3 in [%use bar])

(** A check for function macros *)
let test_function () =
  check int "can be applied" 8 ([%use baz] 4)

let tests = [
  ("use", `Quick, test_use);
  ("binding", `Quick, test_binding);
  ("function", `Quick, test_function)
]

let test_suites: unit test list = [
  "Macro", tests;
]

(** Run the test suites *)
let () = run "ppx_macro" test_suites
