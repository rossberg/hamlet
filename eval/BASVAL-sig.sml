(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML basic values
 *
 * Definition, Section 6.4
 *)

signature BASVAL =
sig
  (* Import *)

  type BasVal = DynamicObjectsCore.BasVal
  type Val    = DynamicObjectsCore.Val


  (* Operations *)

  exception TypeError of string

  val APPLY : BasVal * Val -> Val (* raises Pack TypeError *)

  val toString : BasVal -> string
end;
