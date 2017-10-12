(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML long identifiers
 *
 * Definition, Section 2.4
 *
 * Note:
 *   This is a generic signature to represent all kinds of long identifiers.
 *)

signature LONGID =
sig
  (* Import *)

  structure Id    : ID
  structure StrId : ID

  type Id         = Id.Id
  type StrId      = StrId.Id


  (* Type [Section 2.4] *)

  eqtype longId                                 (* [longid] *)


  (* Operations *)

  val invent     : unit -> longId
  val fromId     : Id -> longId
  val toId       : longId -> Id
  val toString   : longId -> string

  val strengthen : StrId * longId -> longId
  val implode    : StrId list * Id -> longId
  val explode    : longId -> StrId list * Id

  val isShort    : longId -> bool

  val compare    : longId * longId -> order
end;
