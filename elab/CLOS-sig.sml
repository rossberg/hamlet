(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML closure of value environments
 *
 * Definition, Section 4.7 and 4.8
 *)

signature CLOS =
sig
  (* Import *)

  type ValBind = SyntaxCore.ValBind
  type ValEnv  = StaticObjectsCore.ValEnv

  type Context = StaticObjectsCore.Context


  (* Operation *)

  val Clos : Context * ValBind -> ValEnv -> ValEnv
end;
