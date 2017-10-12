(*
 * (c) Andreas Rossberg 2007
 *
 * Auxiliaries for printing grammar
 *)

signature PP_GRAMMAR =
sig
    type outstream = TextIO.outstream

    val ppAtom : outstream * int * string * string -> unit
    val ppElem : outstream * int * string * Source.info *
		     (outstream * int -> unit) list -> unit

    val sub :    (outstream * int * 'a -> unit) -> 'a ->
		     outstream * int -> unit
    val subs :   (outstream * int * 'a -> unit) -> 'a list ->
		     outstream * int -> unit
    val subo :   (outstream * int * 'a -> unit) -> 'a option ->
		     outstream * int -> unit
end;
