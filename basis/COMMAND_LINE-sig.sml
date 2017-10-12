(*
 * (c) Andreas Rossberg 2013
 *
 * Standard ML Basis Library
 *)

signature COMMAND_LINE =
sig
  val name : unit -> string
  val arguments : unit -> string list
end;
