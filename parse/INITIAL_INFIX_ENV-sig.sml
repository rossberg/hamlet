(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML initial infix environment
 *
 * Definition, Appendix C
 *)

signature INITIAL_INFIX_ENV =
sig
    (* Import type *)

    type InfEnv = Infix.InfEnv

    (* Export *)

    val J0 : InfEnv
end;
