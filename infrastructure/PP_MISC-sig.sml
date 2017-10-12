(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML miscellaneous pretty printing helpers
 *)

signature PP_MISC =
sig
  type doc = PrettyPrint.doc

  val nest        : doc -> doc

  val paren       : doc -> doc
  val brace       : doc -> doc
  val brack       : doc -> doc
  val comment     : doc -> doc

  val parenAt     : int -> int * doc -> doc

  val ppList      : ('a -> doc) -> 'a list -> doc
  val ppCommaList : ('a -> doc) -> 'a list -> doc
  val ppStarList  : ('a -> doc) -> 'a list -> doc
  val ppSeq       : ('a -> doc) -> 'a list -> doc
  val ppSeqPrec   : (int -> 'a -> doc) -> int -> 'a list -> doc
end;
