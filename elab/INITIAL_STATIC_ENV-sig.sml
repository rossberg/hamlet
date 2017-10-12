(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML core view of the initial static basis
 *
 * Definition, Appendix C
 *)

signature INITIAL_STATIC_ENV =
sig
  (* Import *)

  type Type             = StaticObjectsCore.Type
  type TyName           = StaticObjectsCore.TyName
  type TyNameSet        = StaticObjectsCore.TyNameSet
  type OverloadingClass = StaticObjectsCore.OverloadingClass
  type Env              = StaticObjectsCore.Env


  (* Predefined monomorphic types [Figure 24] *)

  val tBool     : TyName
  val tInt      : TyName
  val tWord     : TyName
  val tReal     : TyName
  val tString   : TyName
  val tChar     : TyName
  val tExn      : TyName

  val tauBool   : Type
  val tauInt    : Type
  val tauWord   : Type
  val tauReal   : Type
  val tauString : Type
  val tauChar   : Type
  val tauExn    : Type

  (* Initial environment [Appendix C] *)

  val T0        : TyNameSet
  val E0        : Env
end;
