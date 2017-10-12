(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML Basis Library `use' function
 *)

signature USE =
sig
    val enqueue : string -> unit		(* may raise Path *)
    val extract : unit -> string option
end;
