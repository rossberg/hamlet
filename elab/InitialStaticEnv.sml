(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML core view of the initial static basis
 *
 * Definition, Appendix C
 * + RFC: Transformation patterns
 * + RFC: Views
 * + RFC: Nested signatures
 *)

structure InitialStaticEnv : INITIAL_STATIC_ENV =
struct
    (* Import *)

    open StaticObjectsCore
    open IdStatus


    (* VIds [Figure 25; RFC: Transformation patterns] *)

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


    (* TyCons [Figure 24; RFC: Transformation patterns] *)

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


    (* TyNames [Appendix C; RFC: Transformation patterns] *)

    val tBool   = TyName.tyname(TyCon.toString tyconBool,   0, true,  2)
    val tInt    = TyName.tyname(TyCon.toString tyconInt,    0, true,  0)
    val tWord   = TyName.tyname(TyCon.toString tyconWord,   0, true,  0)
    val tReal   = TyName.tyname(TyCon.toString tyconReal,   0, false, 0)
    val tString = TyName.tyname(TyCon.toString tyconString, 0, true,  0)
    val tChar   = TyName.tyname(TyCon.toString tyconChar,   0, true,  0)
    val tOption = TyName.tyname(TyCon.toString tyconOption, 1, true,  2)
    val tList   = TyName.tyname(TyCon.toString tyconList,   1, true,  2)
    val tRef    = TyName.tyname(TyCon.toString tyconRef,    1, true,  1)
    val tExn    = TyName.tyname(TyCon.toString tyconExn,    0, false, 0)

    val T0      = TyNameSet.fromList[tBool, tInt, tWord, tReal, tString, tChar,
				     tOption, tList, tRef, tExn]


    (* Types *)

    val alpha      = TyVar.fromString "'a"
    val alphaEq    = TyVar.fromString "''a"
    val tauAlpha   = Type.fromTyVar alpha
    val tauAlphaEq = Type.fromTyVar alphaEq

    val tauUnit        = Type.fromRowType Type.emptyRow
    val tauBool        = Type.fromConsType([], tBool)
    val tauInt         = Type.fromConsType([], tInt)
    val tauWord        = Type.fromConsType([], tWord)
    val tauReal        = Type.fromConsType([], tReal)
    val tauString      = Type.fromConsType([], tString)
    val tauChar        = Type.fromConsType([], tChar)
    val tauExn         = Type.fromConsType([], tExn)
    val tauAlphaOption = Type.fromConsType([tauAlpha], tOption)
    val tauAlphaList   = Type.fromConsType([tauAlpha], tList)
    val tauAlphaRef    = Type.fromConsType([tauAlpha], tRef)


    (* TypeSchemes [Figure 25; RFC: Transformation patterns] *)

    fun pairType(tau1,tau2) =
	Type.fromRowType(
	    Type.insertRow(Type.insertRow(Type.emptyRow, Lab.fromInt 1, tau1),
							 Lab.fromInt 2, tau2))
    val funType = Type.fromFunType

    val sigmaEq     = ([alphaEq],
		       funType(pairType(tauAlphaEq,tauAlphaEq), tauBool))
    val sigmaAssign = ([alpha],
		       funType(pairType(tauAlphaRef,tauAlpha), tauUnit))
    val sigmaFalse  = ([], tauBool)
    val sigmaTrue   = ([], tauBool)
    val sigmaNONE   = ([alpha], tauAlphaOption)
    val sigmaSOME   = ([alpha], funType(tauAlpha, tauAlphaOption))
    val sigmaNil    = ([alpha], tauAlphaList)
    val sigmaCons   = ([alpha],
		       funType(pairType(tauAlpha, tauAlphaList), tauAlphaList))
    val sigmaRef    = ([alpha], funType(tauAlpha, tauAlphaRef))

    val sigmaMatch  = ([], tauExn)
    val sigmaBind   = ([], tauExn)


    (* Value entries [Figure 25; RFC: Transformation patterns; RFC: Views] *)

    val valstrEq     = (sigmaEq,     IdStatus v)
    val valstrAssign = (sigmaAssign, IdStatus v)

    val valstrFalse  = (sigmaFalse,  IdStatus c)
    val valstrTrue   = (sigmaTrue,   IdStatus c)
    val valstrNONE   = (sigmaNONE,   IdStatus c)
    val valstrSOME   = (sigmaSOME,   IdStatus c)
    val valstrNil    = (sigmaNil,    IdStatus c)
    val valstrCons   = (sigmaCons,   IdStatus c)
    val valstrRef    = (sigmaRef,    IdStatus c)

    val valstrMatch  = (sigmaMatch,  IdStatus e)
    val valstrBind   = (sigmaBind,   IdStatus e)


    (* TypeFcns [Figure 24; RFC: Transformation patterns] *)

    val thetaUnit   = ([], tauUnit)
    val thetaBool   = ([], tauBool)
    val thetaInt    = ([], tauInt)
    val thetaWord   = ([], tauWord)
    val thetaReal   = ([], tauReal)
    val thetaString = ([], tauString)
    val thetaChar   = ([], tauChar)
    val thetaExn    = ([], tauExn)
    val thetaOption = ([alpha], tauAlphaOption)
    val thetaList   = ([alpha], tauAlphaList)
    val thetaRef    = ([alpha], tauAlphaRef)


    (* TyStrs [Figure 25; RFC: Transformation patterns] *)

    val VEEmpty  = VIdMap.empty
    val VEBool   = VIdMap.fromList[(vidFalse, valstrFalse),
				   (vidTrue,  valstrTrue)] : ValEnv
    val VEOption = VIdMap.fromList[(vidNONE,  valstrNONE),
				   (vidSOME,  valstrSOME)]
    val VEList   = VIdMap.fromList[(vidNil,   valstrNil),
				   (vidCons,  valstrCons)]
    val VERef    = VIdMap.fromList[(vidRef,   valstrRef)]

    val tystrUnit   = (thetaUnit,   VEEmpty)
    val tystrBool   = (thetaBool,   VEBool)
    val tystrInt    = (thetaInt,    VEEmpty)
    val tystrWord   = (thetaWord,   VEEmpty)
    val tystrReal   = (thetaReal,   VEEmpty)
    val tystrString = (thetaString, VEEmpty)
    val tystrChar   = (thetaChar,   VEEmpty)
    val tystrOption = (thetaOption, VEOption)
    val tystrList   = (thetaList,   VEList)
    val tystrRef    = (thetaRef,    VERef)
    val tystrExn    = (thetaExn,    VEEmpty)


    (* Environments [Appendix C; RFC: Nested signatures;
     *                           RFC: Transformation patterns] *)

    val G0  = SigIdMap.empty

    val SE0 = StrIdMap.empty

    val TE0 = TyConMap.fromList[(tyconUnit,   tystrUnit),
 				(tyconBool,   tystrBool),
 				(tyconInt,    tystrInt),
 				(tyconWord,   tystrWord),
 				(tyconReal,   tystrReal),
 				(tyconString, tystrString),
 				(tyconChar,   tystrChar),
 				(tyconOption, tystrOption),
 				(tyconList,   tystrList),
 				(tyconRef,    tystrRef),
 				(tyconExn,    tystrExn)]

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
				(vidBind,   valstrBind)]

    val E0 = Env(G0,SE0,TE0,VE0)
end;
