(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML core elaboration
 *
 * Definition, Sections 4.10, 4.11, 4.6, 4.7, 2.9
 *
 * Notes:
 * - Elaboration also checks the further restrictions [Section 4.11].
 * - To implement the 3rd restriction in 4.11 elabDec is passed an
 *   additional argument to indicate the toplevel.
 *)

signature ELAB_CORE =
sig
  (* Import *)

  type Dec     = SyntaxCore.Dec
  type Ty      = SyntaxCore.Ty

  type Type    = StaticObjectsCore.Type
  type Env     = StaticObjectsCore.Env
  type Context = StaticObjectsCore.Context


  (* Export *)

  datatype level = TOP | INNER

  val elabDec : level -> Context * Dec -> Env
  val elabTy  : Context * Ty -> Type
end;
