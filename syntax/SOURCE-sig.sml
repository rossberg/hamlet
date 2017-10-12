(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Helpers for handling source strings
 *)

signature SOURCE =
sig
  type source = string
  type pos    = int * int
  type region = pos * pos
  type loc    = {file : string option, region : region}

  val nowhere : loc
  val at      : loc -> loc
  val left    : loc -> loc
  val right   : loc -> loc
  val over    : loc * loc -> loc

  val compare : loc * loc -> order
end;
