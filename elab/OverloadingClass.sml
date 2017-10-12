(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML overloading classes
 *
 * Definition, Appendix E
 *
 * Note:
 *  Overloading -- and defaulting in particular -- is not well formalised in
 *  the Definition. We treat overloaded constants and identifiers
 *  uniformingly and tried to smoothly integrate overloading resolution into
 *  type inference by generalising the concept of overloading class a bit.
 *  We describe an overloading class as a pair (T,t) of a set
 *  of type names (like the definition does), plus the default type name t.
 *  For overloading to be sound some well-formedness properties have to be
 *  enforced for all existing overloading classes (T,t):
 *  (1) t elem T
 *  (2) Eq T = 0  \/  t admits equality
 *  (3) forall (T',t') . (TT' = 0  \/  |{t,t'} intersect TT'| = 1)
 *  where Eq T = {t elem T | t admits equality} and we write TT' for
 *  T intersect T' and 0 for the empty set.
 *  The reason for (1) is obvious. (2) guarantees that we do not loose the
 *  default if we enforce equality. (3) ensures a unique default whenever we
 *  have to unify two overloading classes. (2) and (3) also allow the
 *  resulting set to become empty which represents a type error.
 *)

structure OverloadingClass :> OVERLOADINGCLASS =
struct
    (* Import types *)

    type TyName    = TyName.TyName
    type TyNameSet = TyNameSet.set


    (* Type *)

    type OverloadingClass = TyNameSet * TyName		(* [O] *)


    (* Simple operations *)

    fun make O            = O
    fun isEmpty (T,t)     = TyNameSet.isEmpty T
    fun set (T,t)         = T
    fun default (T,t)     = t
    fun member((T,t), t') = TyNameSet.member(T, t')


    (* Filter equality types *)

    fun makeEquality (T,t) =
	let
	    val T' = TyNameSet.filter TyName.admitsEquality T
	in
	    if TyNameSet.isEmpty T' then
		NONE
	    else if TyName.admitsEquality t then
		SOME (T',t)
	    else
		raise Fail "OverloadingClass.makeEquality: \
			   \inconsistent overloading class"
	end


    (* Intersection and union *)

    fun union((T1,t1), (T2,t2)) = ( TyNameSet.union(T1,T2), t2 )

    fun intersection((T1,t1), (T2,t2)) =
	let
	    val T' = TyNameSet.intersection(T1,T2)
	in
	    if TyNameSet.isEmpty T' then
		NONE
	    else if t1 = t2 then
		SOME (T',t1)
	    else case (TyNameSet.member(T',t1), TyNameSet.member(T',t2))
	      of (true, false) => SOME (T',t1)
	       | (false, true) => SOME (T',t2)
	       | _ => raise Fail "OverloadingClass.intersection: \
				 \inconsistent overloading classes"
	end
end;
