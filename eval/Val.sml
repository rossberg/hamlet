(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML values
 *
 * Definition, Sections 6.2, 6.3, and 6.4
 *
 * Note:
 *   - All value types are parameterised over the representation of function
 *     closures to break up the recursion between values and environments.
 *   - The basic values are just represented by strings.
 *)

structure Val :> VAL =
struct
    (* Import *)

    open DynamicObjectsCore


    (* Operations *)

    fun exname(ExName en)	= en
      | exname(ExNameVal(en,v)) = en

    fun toPair(Record r) =
	if LabMap.numItems r <> 2 then NONE else
	(case (LabMap.find(r, Lab.fromInt 1), LabMap.find(r, Lab.fromInt 2))
	   of (SOME v1, SOME v2) => SOME(v1, v2)
	    | _ => NONE
	)
      | toPair _ = NONE

    fun toList v = toList'(v, nil)
    and toList'(VId vid, vs) =
	if vid = VId.fromString "nil" then
	    SOME(List.rev vs)
	else
	    NONE
      | toList'(VIdVal(vid, v), vs) =
	if vid = VId.fromString "::" then
	    case toPair v
	      of SOME(v1, v2) => toList'(v2, v1::vs)
	       | NONE         => NONE
	else
	    NONE
      | toList'(_, vs) = NONE


    (* Implementation of polymorphic equality *)

    fun equal(SVal sv1,          SVal sv2         ) = SVal.equal(sv1, sv2)
      | equal(VId vid1,          VId vid2         ) = vid1 = vid2
      | equal(ExVal(ExName en1), ExVal(ExName en2)) = en1 = en2
      | equal(Addr a1,           Addr a2          ) = a1 = a2

      | equal(VIdVal(vid1, v1), VIdVal(vid2, v2)) =
	    vid1 = vid2 andalso equal(v1, v2)

      | equal(ExVal(ExNameVal(en1,v1)), ExVal(ExNameVal(en2,v2))) =
	    en1 = en2 andalso equal(v1, v2)

      | equal(Record r1, Record r2) =
	    LabMap.numItems r1 = LabMap.numItems r2 andalso
	    LabMap.alli (fn(lab, v1) =>
			   case LabMap.find(r2, lab)
			     of SOME v2 => equal(v1, v2)
			      | NONE    => false
			) r1

      | equal _ = false
end;
