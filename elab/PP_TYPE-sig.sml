(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of types and type schemes
 *)

signature PP_TYPE =
sig
  type Type        = StaticObjectsCore.Type
  type TypeScheme  = StaticObjectsCore.TypeScheme

  val ppType       : Type -> PrettyPrint.doc
  val ppTypeScheme : TypeScheme -> PrettyPrint.doc
end;
