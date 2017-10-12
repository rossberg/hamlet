(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML state
 *
 * Definition, Section 6.3
 *)

signature STATE =
sig
  (* Import *)

  type Addr   = DynamicObjectsCore.Addr
  type ExName = DynamicObjectsCore.ExName
  type Val    = DynamicObjectsCore.Val
  type State  = DynamicObjectsCore.State


  (* Operations *)

  val insertAddr   : State * Addr * Val -> State
  val insertExName : State * ExName -> State

  val findAddr     : State * Addr -> Val option
end;
