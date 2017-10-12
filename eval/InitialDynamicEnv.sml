(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML core view of the initial dynamic basis
 *
 * Definition, Appendix D and Section 6.5
 * + RFC: Semantic fixes
 * + RFC: Transformation patterns
 * + RFC: Views
 * + RFC: Nested signatures
 *
 * Note:
 *     The Definition does not specify what the initial state has to contain.
 *     This is a bug as it must at least contain the exception names Match
 *     and Bind. We put the state associated with the initial environment in
 *     here, too.
 *)

structure InitialDynamicEnv : INITIAL_DYNAMIC_ENV =
struct
    (* Import *)

    open DynamicObjectsCore
    open IdStatus


    (* VIds [Appendix D; RFC: Transformation patterns] *)

    val vidEq     = VId.fromString "="
    val vidAssign = VId.fromString ":="

    val vidFalse  = VId.fromString "false"
    val vidTrue   = VId.fromString "true"
    val vidNONE   = VId.fromString "NONE"
    val vidSOME   = VId.fromString "SOME"
    val vidNil    = VId.fromString "nil"
    val vidCons   = VId.fromString "::"
    val vidRef    = VId.fromString "ref"

    val vidMatch  = VId.fromString "Match"
    val vidBind   = VId.fromString "Bind"


    (* Basic exception names [Section 6.5] *)

    val enMatch = ExName.exname vidMatch
    val enBind  = ExName.exname vidBind


    (* Value entries [Appendix D; RFC: Transformation patterns] *)

    val valstrEq     = (BasVal "=", IdStatus v)
    val valstrAssign = (Assign,     IdStatus v)

    val valstrFalse  = (VId vidFalse, IdStatus c)
    val valstrTrue   = (VId vidTrue,  IdStatus c)
    val valstrNONE   = (VId vidNONE,  IdStatus c)
    val valstrSOME   = (VId vidSOME,  IdStatus c)
    val valstrNil    = (VId vidNil,   IdStatus c)
    val valstrCons   = (VId vidCons,  IdStatus c)
    val valstrRef    = (VId vidRef,   IdStatus c)

    val valstrMatch  = (ExVal(ExName enMatch), IdStatus e)
    val valstrBind   = (ExVal(ExName enBind),  IdStatus e)


    (* TyCons [Figure 26; RFC: Transformation patterns] *)

    val tyconUnit   = TyCon.fromString "unit"
    val tyconBool   = TyCon.fromString "bool"
    val tyconInt    = TyCon.fromString "int"
    val tyconWord   = TyCon.fromString "word"
    val tyconReal   = TyCon.fromString "real"
    val tyconString = TyCon.fromString "string"
    val tyconChar   = TyCon.fromString "char"
    val tyconOption = TyCon.fromString "option"
    val tyconList   = TyCon.fromString "list"
    val tyconRef    = TyCon.fromString "ref"
    val tyconExn    = TyCon.fromString "exn"


    (* Type ValEnvs [Figure 26; RFC: Transformation patterns] *)

    val VEUnit   = VIdMap.empty
    val VEBool   = VIdMap.fromList[(vidFalse, valstrFalse),
				   (vidTrue,  valstrTrue)] : ValEnv
    val VEInt    = VIdMap.empty
    val VEWord   = VIdMap.empty
    val VEReal   = VIdMap.empty
    val VEString = VIdMap.empty
    val VEChar   = VIdMap.empty
    val VEOption = VIdMap.fromList[(vidNONE,  valstrNONE),
				   (vidSOME,  valstrSOME)] : ValEnv
    val VEList   = VIdMap.fromList[(vidNil,   valstrNil),
				   (vidCons,  valstrCons)] : ValEnv
    val VERef    = VIdMap.fromList[(vidRef,   valstrRef)] : ValEnv
    val VEExn    = VIdMap.empty


    (* Environments [Appendix D; RFC: Nested signatures;
     *                           RFC: Transformation patterns] *)

    val G0  = SigIdMap.empty

    val SE0 = StrIdMap.empty

    val TE0 = TyConMap.fromList[(tyconUnit,   VEUnit),
 				(tyconBool,   VEBool),
 				(tyconInt,    VEInt),
 				(tyconWord,   VEWord),
 				(tyconReal,   VEReal),
 				(tyconString, VEString),
 				(tyconChar,   VEChar),
 				(tyconOption, VEOption),
 				(tyconList,   VEList),
 				(tyconRef,    VERef),
 				(tyconExn,    VEExn)]

    val VE0 = VIdMap.fromList  [(vidEq,     valstrEq),
				(vidAssign, valstrAssign),
				(vidRef,    valstrRef),
				(vidNil,    valstrNil),
				(vidCons,   valstrCons),
				(vidNONE,   valstrNONE),
				(vidSOME,   valstrSOME),
				(vidFalse,  valstrFalse),
				(vidTrue,   valstrTrue),
				(vidMatch,  valstrMatch),
				(vidBind,   valstrBind)] : ValEnv

    val E0 = Env(G0,SE0,TE0,VE0)


    (* State [RFC: Semantic fixes] *)

    val mem0 = AddrMap.empty
    val ens0 = ExNameSet.fromList[enMatch, enBind]

    val s0 = (mem0, ens0)
end;
