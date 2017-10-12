(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML finite maps
 *
 * Definition, Section 4.2
 *)

functor FinMapFn(Key : ORD_KEY) :>
  FIN_MAP where type Key.ord_key = Key.ord_key =
struct
  structure BinaryMap = BinaryMapFn(Key)
  open BinaryMap

  exception NotFound = LibBase.NotFound
  exception Duplicate

  fun fromList kvs =
      List.foldl
        (fn((k, v), m) =>
          if inDomain(m, k) then raise Duplicate else insert(m, k, v)
        ) empty kvs

  fun extend(m, (k, v)) = insert(m, k, v)
  fun delete(m, k)      = #1(remove(m, k)) handle LibBase.NotFound => m
  fun difference(m, n)  = filteri (fn(k, _) => not(inDomain(n, k))) m

  fun all p     = foldl (fn(v, b) => b andalso p v) true
  fun exists p  = foldl (fn(v, b) => b orelse p v) false
  fun alli p    = foldli (fn(k, v, b) => b andalso p(k, v)) true
  fun existsi p = foldli (fn(k, v, b) => b orelse p(k, v)) false

  fun disjoint(m1, m2) = isEmpty(intersectWith #2 (m1, m2))
end;
