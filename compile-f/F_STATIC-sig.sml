(*
 * (c) Andreas Rossberg 2013
 *
 * System F static semantics.
 *)

signature F_STATIC =
sig
    type kind    = FSyntax.kind
    type typ     = FSyntax.typ
    type value   = FSyntax.value
    type exp     = FSyntax.exp

    type typ_env = kind VarMap.map         (* [D] *)
    type val_env = typ VarMap.map          (* [G] *)

    exception Error of string

    val eqTyp :   typ -> typ -> bool
    val elabTyp : typ_env * typ -> kind
    val elabVal : typ_env * val_env * value -> typ
    val elabExp : typ_env * val_env * exp -> typ
end;
