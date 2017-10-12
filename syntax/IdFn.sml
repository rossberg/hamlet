(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML identifiers
 *
 * Definition, Section 2.4
 *
 * Note:
 *   This is a generic functor to represent all kinds of identifiers (except
 *   labels and tyvars).
 *)


functor IdFn() :> ID =
struct
  (* Type [Section 2.4] *)

  type Id = string                              (* [id] *)


  (* Creation *)

  fun invent() = "_id" ^ Stamp.toString(Stamp.stamp())
  fun isInvented id = String.sub(id, 0) = #"_"
  fun fromString s = s
  fun toString s = s


  (* Ordering *)

  val compare = String.compare
end;
