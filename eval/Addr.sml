(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML addresses and maps thereof
 *
 * Definition, Sections 6.2 and 6.3
 *)

structure Addr :> ADDR =
struct
  (* Type [Section 6.2] *)

  type Addr = Stamp.stamp                                       (* [a] *)

  (* Operations *)

  val addr    = Stamp.stamp
  val compare = Stamp.compare
end

structure AddrMap =
    FinMapFn(type ord_key = Addr.Addr; val compare = Addr.compare);
