(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML finite sets
 *
 * Definition, Section 4.2
 *
 * Note:
 *     This signature just extends the one available in the SML/NJ lib.
 *     Actually, the operation added here would be general purpose and useful enough
 *     to be in the lib. Also see FIN_MAP.
 *)

signature FIN_SET =
sig
    include ORD_SET

    exception NotFound

    val fromList : item list -> set
end;
