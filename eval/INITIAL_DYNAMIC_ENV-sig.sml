(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML core view of the initial dynamic basis
 *
 * Definition, Appendix D and Section 6.5
 *
 * Note:
 *   The Definition does not specify what the initial state has to contain.
 *   This is a bug as it must at least contain the exception names Match
 *   and Bind. We put the state associated with the initial environment in
 *   here, too.
 *)

signature INITIAL_DYNAMIC_ENV =
sig
  (* Import *)

  type Env    = DynamicObjectsCore.Env
  type ExName = DynamicObjectsCore.ExName
  type State  = DynamicObjectsCore.State


  (* Basic exception names [Section 6.5] *)

  val enMatch : ExName
  val enBind  : ExName

  (* Initial environment [Appendix D] *)

  val E0 : Env

  (* Associated state *)

  val s0 : State
end;
