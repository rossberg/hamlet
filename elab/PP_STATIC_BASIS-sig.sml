(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of the static basis
 *)

signature PP_STATIC_BASIS =
sig
  type Basis   = StaticObjectsModule.Basis
  type SigEnv  = StaticObjectsModule.SigEnv
  type FunEnv  = StaticObjectsModule.FunEnv

  val ppBasis  : Basis  -> PrettyPrint.doc
  val ppSigEnv : SigEnv -> PrettyPrint.doc
  val ppFunEnv : FunEnv -> PrettyPrint.doc
end;
