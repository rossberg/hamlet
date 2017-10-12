(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML finite sets
 *
 * Definition, Section 4.2
 *
 * Note:
 *     This functor just extends the one available in the SML/NJ lib.
 *     Actually, the operation added here would be general purpose and useful enough
 *     to be in the lib. Also see FinMapFn.
 *)

functor FinSetFn(Key : ORD_KEY) :>
FIN_SET where type Key.ord_key = Key.ord_key =
struct
    structure BinarySet	= BinarySetFn(Key)
    open BinarySet

    exception NotFound	= LibBase.NotFound

    fun fromList xs	= addList(empty, xs)
end;
