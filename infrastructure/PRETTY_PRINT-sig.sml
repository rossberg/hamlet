(*
 * (c) Andreas Rossberg 1999-2013
 *
 * A generic pretty printer.
 *
 * Based on:
 *    Philip Wadler. "A prettier printer"
 *    http://cm.bell-labs.com/cm/cs/who/wadler/
 * and Christian Lindig's port to OCaml.
 *
 * The semantics has been extended to allow 4 different kinds of
 * groups (`boxes'), 2 modes of nesting, and varying break representations.
 * This is no longer easily described by an algebra though, and the `below'
 * combinator looses optimality.
 *)

signature PRETTY_PRINT =
sig
  type doc

  val empty    : doc                    (* empty document *)
  val break    : doc                    (* space or line break *)
  val ebreak   : doc                    (* empty or line break *)
  val text     : string -> doc          (* raw text *)

  val ^^       : doc * doc -> doc       (* concatenation *)
  val ^/^      : doc * doc -> doc       (* concatenation with break *)

  val hbox     : doc -> doc             (* horizontal box *)
  val vbox     : doc -> doc             (* vertical box *)
  val fbox     : doc -> doc             (* fill box (h and v) *)
  val abox     : doc -> doc             (* auto box (h or v) *)

  val nest     : int -> doc -> doc      (* indentation by k char's *)
  val below    : doc -> doc             (* keep current indentation *)

  val isEmpty  : doc -> bool

  val toString : doc * int -> string
  val output   : TextIO.outstream * doc * int -> unit
end;
