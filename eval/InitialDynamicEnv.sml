(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML core view of the initial dynamic basis
 *
 * Definition, Appendix D and Section 6.5
 *
 * Notes: see INITAL_DYNAMIC_ENV-sig.sml
 *)

structure InitialDynamicEnv : INITIAL_DYNAMIC_ENV =
struct
  (* Import *)

  open DynamicObjectsCore
  open IdStatus


  (* VIds [Appendix D] *)

  val vidEq     = VId.fromString "="
  val vidAssign = VId.fromString ":="

  val vidFalse  = VId.fromString "false"
  val vidTrue   = VId.fromString "true"
  val vidNil    = VId.fromString "nil"
  val vidCons   = VId.fromString "::"
  val vidRef    = VId.fromString "ref"

  val vidMatch  = VId.fromString "Match"
  val vidBind   = VId.fromString "Bind"


  (* Basic exception names [Section 6.5] *)

  val enMatch = ExName.exname vidMatch
  val enBind  = ExName.exname vidBind


  (* Value entries [Appendix D] *)

  val valstrEq     = (BasVal "=", v)
  val valstrAssign = (Assign,     v)

  val valstrFalse  = (VId vidFalse, c)
  val valstrTrue   = (VId vidTrue,  c)
  val valstrNil    = (VId vidNil,   c)
  val valstrCons   = (VId vidCons,  c)
  val valstrRef    = (VId vidRef,   c)

  val valstrMatch  = (ExVal(ExName enMatch), e)
  val valstrBind   = (ExVal(ExName enBind),  e)


  (* TyCons [Figure 26] *)

  val tyconUnit   = TyCon.fromString "unit"
  val tyconBool   = TyCon.fromString "bool"
  val tyconInt    = TyCon.fromString "int"
  val tyconWord   = TyCon.fromString "word"
  val tyconReal   = TyCon.fromString "real"
  val tyconString = TyCon.fromString "string"
  val tyconChar   = TyCon.fromString "char"
  val tyconList   = TyCon.fromString "list"
  val tyconRef    = TyCon.fromString "ref"
  val tyconExn    = TyCon.fromString "exn"


  (* Type ValEnvs [Figure 26] *)

  val VEUnit   = VIdMap.empty
  val VEBool   = VIdMap.fromList[(vidFalse, valstrFalse), (vidTrue, valstrTrue)]
  val VEInt    = VIdMap.empty
  val VEWord   = VIdMap.empty
  val VEReal   = VIdMap.empty
  val VEString = VIdMap.empty
  val VEChar   = VIdMap.empty
  val VEList   = VIdMap.fromList[(vidNil, valstrNil), (vidCons, valstrCons)]
  val VERef    = VIdMap.fromList[(vidRef, valstrRef)]
  val VEExn    = VIdMap.empty


  (* Environments [Appendix D] *)

  val SE0 = StrIdMap.empty

  val TE0 =
      TyConMap.fromList[
        (tyconUnit,   VEUnit),
        (tyconBool,   VEBool),
        (tyconInt,    VEInt),
        (tyconWord,   VEWord),
        (tyconReal,   VEReal),
        (tyconString, VEString),
        (tyconChar,   VEChar),
        (tyconList,   VEList),
        (tyconRef,    VERef),
        (tyconExn,    VEExn)
      ]

  val VE0 =
      VIdMap.fromList[
        (vidEq,     valstrEq),
        (vidAssign, valstrAssign),
        (vidRef,    valstrRef),
        (vidNil,    valstrNil),
        (vidCons,   valstrCons),
        (vidFalse,  valstrFalse),
        (vidTrue,   valstrTrue),
        (vidMatch,  valstrMatch),
        (vidBind,   valstrBind)
      ]

  val E0 = Env(SE0, TE0, VE0)


  (* Associated state *)

  val mem0 = AddrMap.empty
  val ens0 = ExNameSet.fromList[enMatch, enBind]

  val s0 = (mem0, ens0)
end;
