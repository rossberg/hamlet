(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library primitives
 *
 * Definition, Sections 6.2, 6.4 and Appendix D; Standard Basis Specification
 *)

signature DYNAMIC_LIBRARY =
sig
  (* Import *)

  type Val    = DynamicObjectsCore.Val
  type BasVal = DynamicObjectsCore.BasVal
  type State  = DynamicObjectsCore.State
  type Basis  = DynamicObjectsModule.Basis


  (* Value class representation [Section 6.2] *)

  include LIBRARY_SVAL

  (* Initial basis [Appendix D] *)

  val B0 : Basis
  val s0 : State

  (* APPLY function [Section 6.4] *)

  exception TypeError of string
  val APPLY : BasVal * Val -> Val
end;
