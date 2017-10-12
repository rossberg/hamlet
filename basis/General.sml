(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure General :> GENERAL =
struct
  type unit           = {}
  type exn            = exn

  exception Bind      = Bind
  exception Chr       = Chr
  exception Div       = Div
  exception Domain    = Domain
  exception Fail      = Fail
  exception Match     = Match
  exception Overflow  = Overflow
  exception Size      = Size
  exception Span      = Span
  exception Subscript = Subscript

  val exnName         = use{b = "General.exnName"} : exn -> string
  val exnMessage      = exnName

  datatype order      = LESS | EQUAL | GREATER

  fun !(ref v)        = v
  val op:=            = op:=

  fun (f o g) a       = f(g a)
  fun a before b      = a
  fun ignore a        = () 
end

open General;
