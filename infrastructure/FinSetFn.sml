(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML finite sets
 *
 * Definition, Section 4.2
 *)

functor FinSetFn(Key : ORD_KEY) :>
  FIN_SET where type Key.ord_key = Key.ord_key =
struct
  structure BinarySet = BinarySetFn(Key)
  open BinarySet

  exception NotFound   = LibBase.NotFound

  fun fromList xs      = addList(empty, xs)

  fun first s          = find (fn x => true) s
  fun disjoint(s1, s2) = isEmpty(intersection(s1, s2))
end;
