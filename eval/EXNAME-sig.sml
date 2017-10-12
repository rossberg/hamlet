(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML exception names
 *
 * Definition, Section 6.2
 *)

signature EXNAME =
sig
  (* Import *)

  type VId = VId.Id


  (* Type [Section 6.2] *)

  eqtype ExName                                         (* [en] *)


  (* Operations *)

  val exname   : VId -> ExName
  val toString : ExName -> string

  val compare  : ExName * ExName -> order
end;
