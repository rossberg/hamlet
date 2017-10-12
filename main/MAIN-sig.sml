(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML implementation stand-alone
 *)

signature MAIN =
sig
    val version : string
    val main :    unit -> 'a
end;
