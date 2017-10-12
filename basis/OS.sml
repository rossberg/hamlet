(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML Basis Library
 *
 * Note: Incomplete.
 *)

structure OS :> OS =
struct
    open OS

    type syserror = int
    exception SysErr of string * syserror option	(* defunct dummy *)
end;
