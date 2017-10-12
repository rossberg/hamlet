(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML initial infix environment
 *
 * Definition, Appendix C
 *)

structure InitialInfixEnv : INITIAL_INFIX_ENV =
struct
  (* Import type *)

  type InfEnv = Infix.InfEnv

  (* Value identifiers *)

  val vidCons   = VId.fromString "::"
  val vidEqual  = VId.fromString "="
  val vidAssign = VId.fromString ":="

  (* Export *)

  val J0 =
      VIdMap.fromList[
        (vidCons,   (Infix.RIGHT, 5)),
        (vidEqual,  (Infix.LEFT,  4)),
        (vidAssign, (Infix.LEFT,  3))
      ]
end;
