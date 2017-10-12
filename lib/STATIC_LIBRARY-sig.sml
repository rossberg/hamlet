(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library primitives
 *
 * Definition, Appendix C and E; Standard Basis Specification
 *)

signature STATIC_LIBRARY =
sig
  (* Import *)

  type TyName           = StaticObjectsCore.TyName
  type OverloadingClass = StaticObjectsCore.OverloadingClass
  type Basis            = StaticObjectsModule.Basis


  (* Overloading classes [Section E.1] *)

  val tWord8  : TyName
  val tVector : TyName

  val Int     : OverloadingClass
  val Word    : OverloadingClass
  val Real    : OverloadingClass
  val String  : OverloadingClass
  val Char    : OverloadingClass

  (* Initial basis [Appendix C] *)

  val B0      : Basis
end;
