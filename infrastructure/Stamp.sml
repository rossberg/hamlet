(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Stamp generator.
 *)

structure Stamp :> STAMP =
struct
  type stamp = int

  val r = ref 0

  fun reset()  =  r := 0
  fun stamp()  = (r := !r + 1; !r)

  val toString = Int.toString
  val compare  = Int.compare
  val min      = Int.min
end

structure StampMap =
    FinMapFn(type ord_key = Stamp.stamp; val compare = Stamp.compare);
