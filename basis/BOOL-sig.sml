(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure StringCvt = struct type ('a, 'b) reader = 'b -> ('a * 'b) option end

signature BOOL =
sig
  datatype bool = datatype bool
  val not : bool -> bool
  val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
  val fromString : string -> bool option
  val toString : bool -> string
end;
