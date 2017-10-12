(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML implementation main structure
 *)

signature SML =
sig
    val basisPath :	string ref

    val parseString :	string -> unit	(* Parse only *)
    val elabString :	string -> unit	(* Parse and elaborate *)
    val evalString :	string -> unit	(* Parse and evaluate *)
    val execString :	string -> unit	(* Parse, elaborate, and evaluate *)

    val parseFile :	string -> unit
    val elabFile :	string -> unit
    val evalFile :	string -> unit
    val execFile :	string -> unit

    val parseFiles :	string list -> unit
    val elabFiles :	string list -> unit
    val evalFiles :	string list -> unit
    val execFiles :	string list -> unit

    val parseSession :	unit -> unit
    val elabSession :	unit -> unit
    val evalSession :	unit -> unit
    val execSession :	unit -> unit
end;
