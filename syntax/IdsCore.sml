(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML identifiers for the core and maps thereof
 *
 * Definition, Section 2.4
 *)

structure VId		= IdFn()
structure TyCon		= IdFn()
structure StrId		= IdFn()

structure LongVId	= LongIdFn(structure Id    = VId
				   structure StrId = StrId)
structure LongTyCon	= LongIdFn(structure Id    = TyCon
				   structure StrId = StrId)
structure LongStrId	= LongIdFn(structure Id    = StrId
				   structure StrId = StrId)

structure VIdMap	= FinMapFn(type ord_key = VId.Id
				   val  compare = VId.compare)
structure TyConMap	= FinMapFn(type ord_key = TyCon.Id
				   val  compare = TyCon.compare)
structure StrIdMap	= FinMapFn(type ord_key = StrId.Id
				   val  compare = StrId.compare)

structure IdsCore =
struct
    type VId		= VId.Id
    type TyVar		= TyVar.TyVar
    type TyCon		= TyCon.Id
    type Lab		= Lab.Lab
    type StrId		= StrId.Id

    type longVId	= LongVId.longId
    type longTyCon	= LongTyCon.longId
    type longStrId	= LongStrId.longId

    type 'a VIdMap	= 'a VIdMap.map
    type 'a TyVarMap	= 'a TyVarMap.map
    type 'a TyConMap	= 'a TyConMap.map
    type 'a LabMap	= 'a LabMap.map
    type 'a StrIdMap	= 'a StrIdMap.map
end;
