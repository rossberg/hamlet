(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note: Incomplete.
 *)

signature TEXT_STREAM_IO =
sig
  include STREAM_IO
    where type vector = CharVector.vector
    where type elem = Char.char
(*
  val inputLine : instream -> (string * instream) option
  val outputSubstr : outstream * substring -> unit
*)
end;
