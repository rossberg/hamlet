(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML values
 *
 * Definition, Sections 6.2, 6.3, and 6.4
 *
 * Notes: see DynamicObjectsCore.sml
 *)

signature VAL =
sig
  (* Import *)

  type Val    = DynamicObjectsCore.Val
  type ExVal  = DynamicObjectsCore.ExVal
  type ExName = DynamicObjectsCore.ExName


  (* Operations *)

  val equal    : Val * Val -> bool

  val exname   : ExVal -> ExName

  val fromPair : Val * Val -> Val
  val fromList : Val list -> Val
  val toPair   : Val -> (Val * Val) option
  val toList   : Val -> Val list option
end;
