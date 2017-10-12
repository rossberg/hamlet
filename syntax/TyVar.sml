(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML type variables and sets and maps thereof
 *
 * Definition, Sections 2.4, 4.1, and 4.2
 *
 * Note:
 * - Tyvars can be annotated with an overloading class, to represent the
 *   type schemes of overloaded identifiers.
 * - We wrap the overloading class into a reference so that TyVar is an eqtype.
 * - Internally generated tyvars get names _'xxx, where xxx is a stamp number.
 * - Tyvars generated from integers are mapped to 'a,..,'z,'a1,..,'z1,'a2,...
 *)

structure TyVar :> TYVAR =
struct
  (* Import types *)

  type OverloadingClass = OverloadingClass.OverloadingClass


  (* Type [Sections 2.4 and 4.1]*)

  type TyVar =                                  (* [alpha] or [tyvar] *)
      { name : string,
        equality : bool,
        overloading : OverloadingClass ref option
      }


  (* Creation *)

  fun prefix true  = "''"
    | prefix false = "'"

  fun invent equality =
      let
        val name = "_" ^ prefix equality ^ Stamp.toString(Stamp.stamp())
      in
        {name = name, equality = equality, overloading = NONE}
      end

  fun fromInt equality n =
      let
        val c = String.str(Char.chr(Char.ord #"a" + n mod 26))
        val i = n div 26
        val name = prefix equality ^ (if i = 0 then c else c ^ Int.toString i)
      in
        {name = name, equality = equality, overloading = NONE}
      end

  fun fromString s =
      let
        val equality = String.size s > 1 andalso String.sub(s, 1) = #"'"
      in
        {name = s, equality = equality, overloading = NONE}
      end

  fun fromOverloadingClass(s, O) =
      {name = s, equality = false, overloading = SOME(ref O)}


  (* Attributes [Section 4.1] *)

  fun toString{name, equality, overloading} = name

  fun admitsEquality{name, equality, overloading} = equality

  fun overloadingClass{name, equality, overloading} = Option.map op! overloading


  (* Ordering *)

  fun compare(alpha1 : TyVar, alpha2 : TyVar) =
      String.compare(#name alpha1, #name alpha2)
end

structure TyVarSet =
  FinSetFn(type ord_key = TyVar.TyVar; val compare = TyVar.compare)
structure TyVarMap =
  FinMapFn(type ord_key = TyVar.TyVar; val compare = TyVar.compare);
