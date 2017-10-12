(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

signature GENERAL =
sig
  type unit = {}
  type exn = exn
  exception Bind
  exception Match
  exception Chr
  exception Div
  exception Domain
  exception Fail of string
  exception Overflow
  exception Size
  exception Span
  exception Subscript
  val exnName : exn -> string
  val exnMessage : exn -> string
  datatype order = LESS | EQUAL | GREATER
  val ! : 'a ref -> 'a
  val := : 'a ref * 'a -> unit
  val o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
  val before : 'a * unit -> 'a
  val ignore : 'a -> unit
end;
