(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML finite maps
 *
 * Definition, Section 4.2
 *
 * Note: This signature just extends the one available in the SML/NJ lib.
 *)

signature FIN_MAP =
sig
  include ORD_MAP

  exception NotFound
  exception Duplicate

  val fromList   : (Key.ord_key * 'a) list -> 'a map

  val extend     : 'a map * (Key.ord_key * 'a)  -> 'a map 
  val delete     : 'a map * Key.ord_key -> 'a map
  val difference : 'a map * 'a map -> 'a map

  val all        : ('a -> bool) -> 'a map -> bool
  val exists     : ('a -> bool) -> 'a map -> bool
  val alli       : (Key.ord_key * 'a -> bool) -> 'a map -> bool
  val existsi    : (Key.ord_key * 'a -> bool) -> 'a map -> bool
  val disjoint   : 'a map * 'a map -> bool
end;
