(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML implementation main structure
 *)

signature SML =
sig
  val basisPath        : string option ref

  val parseString      : string -> unit
  val elabString       : string -> unit
  val evalString       : string -> unit
  val execString       : string -> unit
  val compileJSString  : string -> unit

  val parseFile        : string -> unit
  val elabFile         : string -> unit
  val evalFile         : string -> unit
  val execFile         : string -> unit
  val compileJSFile    : string -> unit

  val parseFiles       : string list -> unit
  val elabFiles        : string list -> unit
  val evalFiles        : string list -> unit
  val execFiles        : string list -> unit
  val compileJSFiles   : string list -> unit

  val parseSession     : unit -> unit
  val elabSession      : unit -> unit
  val evalSession      : unit -> unit
  val execSession      : unit -> unit
  val compileJSSession : unit -> unit
end;
