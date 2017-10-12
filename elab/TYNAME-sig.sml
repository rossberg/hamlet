(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML type names
 *
 * Definition, Section 4.1
 *
 * Notes:
 * - Equality is not a boolean attribute. We distinguish a 3rd kind of special
 *   type names which have equality regardless of the types applied. This
 *   implements ref, array, and equivalent types.
 * - For easy checking of pattern exhaustiveness we add an attribute
 *   `span' counting the number of constructors of the type.
 * - For checking of declaration orders etc we provide access to a time stamp.
 *)

signature TYNAME =
sig
  (* Type [Section 4.1] *)

  eqtype TyName                                         (* [t] *)


  (* Operations *)

  val tyname         : string * int * bool * int -> TyName
  val invent         : int * bool -> TyName
  val rename         : TyName -> TyName
  val Abs            : TyName -> TyName

  val arity          : TyName -> int
  val admitsEquality : TyName -> bool
  val span           : TyName -> int
  val toString       : TyName -> string
  val time           : TyName -> Stamp.stamp

  val compare        : TyName * TyName -> order

  val removeEquality : TyName -> unit
  val adjustSpan     : TyName * int -> unit
end;
