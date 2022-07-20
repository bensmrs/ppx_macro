(** This module implements the PPX *)

open Ppxlib
open Ast_builder.Default

(** Macro storage *)
let macros = Hashtbl.create 10

(** Save a macro for later use *)
let save_macro ~loc pat expr =
  Hashtbl.add macros pat expr;
  pstr_eval ~loc (pexp_construct ~loc (Loc.make ~loc (lident "()")) None) []

(** Reuse a macro *)
let use_macro ~loc name =
  {(Hashtbl.find macros name) with pexp_loc = loc}

(** Declare the `macro' extension *)
let macro_mapper =
  Extension.declare "macro" Extension.Context.structure_item
    Ast_pattern.(pstr (pstr_value nonrecursive (value_binding ~pat:(ppat_var __) ~expr:__ ^:: nil)
                       ^:: nil))
    (fun ~loc ~path:_ -> save_macro ~loc)

(** Declare the `use' extension *)
let use_mapper =
  Extension.declare "use" Extension.Context.expression 
    Ast_pattern.(single_expr_payload (pexp_ident (lident __)))
    (fun ~loc ~path:_ -> use_macro ~loc)

(** Register the transformation *)
let () = Driver.register_transformation "macro" ~extensions:[macro_mapper; use_mapper]
