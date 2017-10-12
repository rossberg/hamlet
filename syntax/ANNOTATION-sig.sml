(*
 * (c) Andreas Rossberg 2013
 *
 * Helpers for abstract syntax annotations
 *)

signature ANNOTATION =
sig
  include PROP

  type 'a annotation = (Source.loc, 'a) cons                   (* [A] *)
  datatype ('a, 'b) phrase = @@ of 'a * 'b annotation

  exception Annotation

  val syntax     : ('a, 'b) phrase -> 'a
  val annotation : ('a, 'b) phrase -> 'b annotation

  val loc        : 'a annotation -> Source.loc
  val fromLoc    : Source.loc -> 'a annotation
  val nowhere    : unit -> 'a annotation
  val copy       : 'a annotation -> 'a annotation

  val at         : ('a, 'b) phrase -> 'c annotation
  val left       : ('a, 'b) phrase -> 'c annotation
  val right      : ('a, 'b) phrase -> 'c annotation
  val over       : ('a, 'b) phrase * ('c, 'd) phrase -> 'e annotation
  val overSome   : ('a, 'b) phrase * ('c, 'd) phrase option -> 'e annotation
  val overAll    : ('a, 'b) phrase list -> 'e annotation

  val -->        : 'a * 'a prop -> 'a (* raises Annotation *)
end;
