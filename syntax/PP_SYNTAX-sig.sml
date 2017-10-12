(*
 * (c) Andreas Rossberg 2007-2013
 *
 * Auxiliaries for printing syntax
 *)

signature PP_SYNTAX =
sig
  type outstream = TextIO.outstream

  val ppAtom : outstream * int * string * string -> unit
  val ppElem :
      outstream * int * string * 'a Annotation.annotation *
        (outstream * int -> unit) list -> unit

  val sub :
      (outstream * int * 'a -> unit) -> 'a -> outstream * int -> unit
  val subs :
      (outstream * int * 'a -> unit) -> 'a list -> outstream * int -> unit
  val subo :
      (outstream * int * 'a -> unit) -> 'a option -> outstream * int -> unit
end;
