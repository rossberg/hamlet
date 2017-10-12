(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML identifiers for modules, and sets and maps thereof
 *
 * Definition, Section 3.2
 *)

structure SigId = IdFn()
structure FunId = IdFn()

structure SigIdSet =
  FinSetFn(type ord_key = SigId.Id; val compare = SigId.compare)
structure FunIdSet =
  FinSetFn(type ord_key = FunId.Id; val compare = FunId.compare)

structure SigIdMap =
  FinMapFn(type ord_key = SigId.Id; val compare = SigId.compare)
structure FunIdMap =
  FinMapFn(type ord_key = FunId.Id; val compare = FunId.compare)


structure IdsModule =
struct
  type SigId       = SigId.Id
  type FunId       = FunId.Id

  type SigIdSet    = SigIdSet.set
  type FunIdSet    = FunIdSet.set

  type 'a SigIdMap = 'a SigIdMap.map
  type 'a FunIdMap = 'a FunIdMap.map
end;
