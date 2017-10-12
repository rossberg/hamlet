(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML type variables
 *
 * Definition, Sections 2.4 and 4.1
 *)

signature TYVAR =
sig
  (* Import types *)

  type OverloadingClass = OverloadingClass.OverloadingClass


  (* Type [Sections 2.4 and 4.1]*)

  eqtype TyVar                                  (* [alpha] or [tyvar] *)


  (* Operations *)

  val invent               : bool -> TyVar
  val fromInt              : bool -> int -> TyVar
  val fromString           : string -> TyVar
  val fromOverloadingClass : string * OverloadingClass -> TyVar
  val toString             : TyVar -> string

  val admitsEquality       : TyVar -> bool
  val overloadingClass     : TyVar -> OverloadingClass option

  val compare              : TyVar * TyVar -> order
end;
