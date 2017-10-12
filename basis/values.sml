(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library top-level values
 *
 * Note: Overloaded values are all primitive.
 *)

fun a1 <> a2  = Bool.not(a1 = a2)

open General

val getOpt    = Option.getOpt
val isSome    = Option.isSome
val valOf     = Option.valOf
val not       = Bool.not
val real      = Real.fromInt
val trunc     = Real.trunc
val floor     = Real.floor
val ceil      = Real.ceil
val round     = Real.round
val ord       = Char.ord
val chr       = Char.chr
val size      = String.size
val str       = String.str
val concat    = String.concat
val implode   = String.implode
val explode   = String.explode
val substring = String.substring
val op^       = String.^
val null      = List.null
val hd        = List.hd
val tl        = List.tl
val length    = List.length
val rev       = List.rev
val op@       = List.@
val app       = List.app
val map       = List.map
val foldr     = List.foldr
val foldl     = List.foldl
val print     = TextIO.print
val vector    = Vector.fromList
val use       = use : string -> unit;
