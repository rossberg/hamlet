(*
 * (c) Andreas Rossberg 2013-2025
 *
 * Standard ML Basis Library
 *)

structure CommandLine : COMMAND_LINE =
struct
  val name      = use{b = "CommandLine.name"} : unit -> string
  val arguments = use{b = "CommandLine.arguments"} : unit -> string list
end;
