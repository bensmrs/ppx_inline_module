(** This module implements the PPX *)

open Ppxlib
open Ast_builder.Default

(** Rewrite longidents (Lident and Ldot) to prefix the [%Imod] module *)
let rec rewrite_longident ~loc = function
  | Lident id -> Ldot (Lident "%Imod", id)
  | Ldot (left, right) -> Ldot (rewrite_longident ~loc left, right)
  | _ -> Location.raise_errorf ~loc "Identifier or dotted identifiers required here"

(** Rewrite the [imod] declarations by unpacking and binding the first-class module *)
let rewrite_module expr =
  let loc = expr.pexp_loc in
  let (left, right) = (match expr.pexp_desc with
     | Pexp_field (left, right)
     | Pexp_apply (left, [Nolabel, {pexp_desc = Pexp_ident right; _}]) ->
         let loc = right.loc in
         (left, Loc.make ~loc (rewrite_longident ~loc right.txt))
     | _ -> Location.raise_errorf ~loc
                                  "Expected <first-class module> <DOT|SPACE> <dotted member>") in
  let loc' = left.pexp_loc in
  (left |> pmod_unpack ~loc:loc'
        |> pexp_letmodule ~loc:loc' (Loc.make ~loc:loc' (Some "%Imod"))) (pexp_ident ~loc right)

(** Declare the extension *)
let mapper =
  Extension.declare "imod" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ -> rewrite_module)

(** Register the transformation *)
let () = Driver.register_transformation "inline_module" ~extensions:[mapper]
