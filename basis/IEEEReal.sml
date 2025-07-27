(*
 * (c) Andreas Rossberg 2001-2025
 *
 * Standard ML Basis Library
 *
 * Note: Incomplete.
 *)

structure IEEEReal :> IEEE_REAL =
struct
  exception Unordered

  datatype real_order    = LESS | EQUAL | GREATER | UNORDERED
  datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO
  datatype float_class   = NAN | INF | ZERO | NORMAL | SUBNORMAL
(*
  val setRoundingMode : rounding_mode -> unit
  val getRoundingMode : unit -> rounding_mode
  type decimal_approx =
      {kind : float_class, sign : bool, digits : int list, exp : int}
  val toString : decimal_approx -> string
  val fromString : string -> decimal_approx option
*)
end;
