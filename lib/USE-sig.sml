(*
 * (c) Andreas Rossberg 2001-2025
 *
 * Standard ML Basis Library `use' function
 *)

signature USE =
sig
  val enqueue : string -> unit (* raises Path *)
  val extract : unit -> string option
end;
