(** This module tests the PPX with Alcotest *)

open Alcotest

module type M = sig
  val n : int
  module N : sig
    module O : sig
      val p : int
      val q : int -> int
    end
  end
end

let m = (module struct
  let n = 0
  module N = struct
    module O = struct
      let p = 1
      let q r = 2 * r
    end
  end
end : M)

(** A check for simple expressions *)
let test_simple () =
  check int "can use simple expressions" 0 [%imod m.n] 

(** A check for more complex expressions *)
let test_multiple () =
  check int "can use dotted expressions" 1 [%imod m.N.O.p];
  check int "can use dotted expressions with functions" 6 ([%imod m.N.O.q] 3)

let tests = [
  ("simple", `Quick, test_simple);
  ("multiple", `Quick, test_multiple)
]

let test_suites: unit test list = [
  "Inline", tests;
]

(** Run the test suites *)
let () = run "ppx_inline_module" test_suites
