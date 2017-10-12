(*
 * (c) Andreas Rossberg 2013
 *
 * Free and bound identifiers in the module language.
 *)

signature ID_SET_MODULE =
sig
  type IdSet = {vids : VIdSet.set, strids : StrIdSet.set, funids : FunIdSet.set}

  val boundEnv     : StaticObjectsModule.Env -> IdSet
  val boundBasis   : StaticObjectsModule.Basis -> IdSet
  val boundStrBind : SyntaxModule.StrBind -> StrId.Id list
  val boundFunBind : SyntaxModule.FunBind -> FunId.Id list

  val freeStrDec   : SyntaxModule.StrDec -> IdSet
  val freeFunDec   : SyntaxModule.FunDec -> IdSet
  val freeTopDec   : SyntaxModule.TopDec -> IdSet
  val freeStrBind  : SyntaxModule.StrBind -> IdSet
  val freeFunBind  : SyntaxModule.FunBind -> IdSet

  val disjoint     : IdSet * IdSet -> bool
end;
