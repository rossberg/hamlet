(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML initial static basis
 *
 * Definition, Appendices C and E
 *)

signature INITIAL_STATIC_BASIS =
sig
  (* Import *)

  type Basis = StaticObjectsModule.Basis

  (* Export *)

  val B0 : Basis
end;
