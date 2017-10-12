(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML type names and sets and maps thereof
 *
 * Definition, Sections 4.1, 4.2, 5.1 and 5.2
 *
 * Notes: see TYNAME-sig.sml
 *)

structure TyName :> TYNAME =
struct
  (* Type [Section 4.1] *)

  type TyName =                               (* [t] *)
      { stamp    : Stamp.stamp,
        tycon    : string,
        arity    : int,
        equality : bool ref,
        span     : int ref
      }


  (* Creation *)

  fun tyname(tycon, arity, equality, span) =
      { stamp    = Stamp.stamp(),
        tycon    = tycon,
        arity    = arity,
        equality = ref equality,
        span     = ref span
      }

  fun invent(arity, equality) =
        tyname("_id" ^ Stamp.toString(Stamp.stamp()), arity, equality, 0)


  (* Attributes [Section 4.1] *)

  fun toString(t : TyName)       = #tycon t
  fun time(t : TyName)           = #stamp t
  fun arity(t : TyName)          = #arity t
  fun admitsEquality(t : TyName) = !(#equality t)
  fun span(t : TyName)           = !(#span t)


  (* Creation from existing *)

  fun rename t = tyname(#tycon t, arity t, admitsEquality t, span t)
  fun Abs t    = tyname(#tycon t, arity t, false, 0)


  (* Modification *)

  fun removeEquality(t : TyName) = #equality t := false
  fun adjustSpan(t : TyName, n)  = #span t := n


  (* Ordering *)

  fun compare(t1 : TyName, t2 : TyName) = Stamp.compare(time t1, time t2)
end

structure TyNameSet =
    FinSetFn(type ord_key = TyName.TyName; val  compare = TyName.compare)
structure TyNameMap =
    FinMapFn(type ord_key = TyName.TyName; val  compare = TyName.compare);
