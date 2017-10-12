(*
 * (c) Andreas Rossberg 2013
 *
 * Free and bound identifiers in the core language.
 *)

signature ID_SET_CORE =
sig
  type IdSet = {vids : VIdSet.set, strids : StrIdSet.set}

  val boundEnv       : StaticObjectsCore.Env -> IdSet
  val boundValBind   : SyntaxCore.ValBind -> VIdSet.set list
  val boundExBind    : SyntaxCore.ExBind -> VId.Id list

  val freeDec        : SyntaxCore.Dec -> IdSet
  val freeValBind    : SyntaxCore.ValBind -> IdSet
  val freeExBind     : SyntaxCore.ExBind -> IdSet
  val freeLongStrId  : SyntaxCore.longStrId -> IdSet
  val freeLongStrIds : SyntaxCore.longStrId list -> IdSet

  val disjoint       : IdSet * IdSet -> bool
end;
