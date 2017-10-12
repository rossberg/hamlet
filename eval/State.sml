(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML state
 *
 * Definition, Section 6.3
 *)

structure State :> STATE =
struct
  (* Import *)

  type Addr   = DynamicObjectsCore.Addr
  type ExName = DynamicObjectsCore.ExName
  type Val    = DynamicObjectsCore.Val
  type State  = DynamicObjectsCore.State


  (* Operations *)

  fun insertAddr((mem, ens), a, v) = (AddrMap.insert(mem, a, v), ens)
  fun insertExName((mem, ens), en) = (mem, ExNameSet.add(ens, en))

  fun findAddr((mem, ens), a) = AddrMap.find(mem, a)
end;
