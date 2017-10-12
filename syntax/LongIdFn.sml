(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML long identifiers
 *
 * Definition, Section 2.4
 *
 * Note:
 *   This is a generic functor that generates a long identifier type from a
 *   given identifier type and the StrId type.
 *)

functor LongIdFn(
  structure Id    : ID
  structure StrId : ID
) :> LONGID
  where type Id.Id = Id.Id
  and   type StrId.Id = StrId.Id
=
struct
  (* Import *)

  structure Id    = Id
  structure StrId = StrId

  type Id    = Id.Id
  type StrId = StrId.Id


  (* Type [Section 2.4] *)

  type longId = StrId list * Id                 (* [longid] *)


  (* Conversions *)

  fun toId(strid, id) = id
  fun fromId id = ([], id)
  fun invent() = ([], Id.invent())

  fun toString(strids, id) =
      let
        fun prefix   []     = Id.toString id
          | prefix(id::ids) = StrId.toString id ^ "." ^ prefix ids
      in
        prefix strids
      end

  fun strengthen(strid, (strids, id)) = (strid::strids, id)

  fun implode longid = longid
  fun explode longid = longid

  fun isShort (strids, id) = List.null strids


  (* Ordering *)

  fun compare(longid1, longid2) =
      String.compare(toString longid1, toString longid2)
end;
