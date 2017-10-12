(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML identifiers for modules and maps thereof
 *
 * Definition, Section 3.2
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

structure SigId		= IdFn()
(* Removed FunId [RFC: Higher-order functors] *)

(* [RFC: Nested signatures] *)
structure LongSigId	= LongIdFn(structure Id    = SigId
				   structure StrId = StrId)

structure SigIdMap	= FinMapFn(type ord_key = SigId.Id
				   val  compare = SigId.compare)

structure IdsModule =
struct
    type SigId		= SigId.Id
    (* Removed FunId [RFC: Higher-order functors] *)

    (* [RFC: Nested signatures] *)
    type longSigId	= LongSigId.longId

    type 'a SigIdMap	= 'a SigIdMap.map
    (* Removed FunIdMap [RFC: Higher-order functors] *)
end;
