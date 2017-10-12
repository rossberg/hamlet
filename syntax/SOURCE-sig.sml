(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Helpers for handling source strings
 *)

signature SOURCE =
sig
    type source = string
    type pos    = int * int
    type region = pos * pos
    type info   = {file : string option, region : region}

    exception Error of (int * int) * string

    val nowhere :  info
    val over :     info * info -> info
    val between :  info * info -> info
    val compare :  info * info -> order
end;
