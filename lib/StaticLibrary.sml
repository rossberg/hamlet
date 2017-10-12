(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library primitives
 *
 * Definition, Sections 6.2, 6.4 and Appendices C, D, and E; Standard Basis
 * Specification
 *
 * Notes: see DynamicLibrary.sml
 *)

structure StaticLibrary : STATIC_LIBRARY =
struct
  (* Import *)

  open StaticObjectsCore
  open StaticObjectsModule
  open InitialStaticEnv
  open IdsLibrary


  (* Static objects for the Core *)

  (* Types *)

  val tWord8  = LibrarySVal.tWord8
  val tVector = TyName.tyname(TyCon.toString tyconVector, 1, true, 0)

  val thetaWord8  = TypeFcn.fromTyName tWord8
  val thetaVector = TypeFcn.fromTyName tVector

  (* Overloading classes [Section E.1] *)

  val Int     = OverloadingClass.make(TyNameSet.fromList[tInt], tInt)
  val Real    = OverloadingClass.make(TyNameSet.fromList[tReal], tReal)
  val Word    = OverloadingClass.make(TyNameSet.fromList[tWord, tWord8], tWord)
  val String  = OverloadingClass.make(TyNameSet.fromList[tString], tString)
  val Char    = OverloadingClass.make(TyNameSet.fromList[tChar], tChar)
  val WordInt = OverloadingClass.union(Word, Int)
  val RealInt = OverloadingClass.union(Real, Int)
  val Num     = OverloadingClass.union(Word, RealInt)
  val Txt     = OverloadingClass.union(String, Char)
  val NumTxt  = OverloadingClass.union(Txt, Num)

  (* Type Schemes *)

  fun pairType tau = Type.fromTupleType[tau, tau]

  val sigmaExn = ([], tauExn)

  val rhoIo =
      Type.rowFromList[
        (Lab.fromString "name", tauString),
        (Lab.fromString "function", tauString),
        (Lab.fromString "cause", tauExn)
      ]
  val tauIo         = Type.fromFunType(Type.fromRowType rhoIo, tauExn)
  val sigmaIo       = ([], tauIo)

  val alphaReal     = TyVar.fromOverloadingClass("Real",    Real)
  val alphaRealInt  = TyVar.fromOverloadingClass("realint", RealInt)
  val alphaWordInt  = TyVar.fromOverloadingClass("wordint", WordInt)
  val alphaNum      = TyVar.fromOverloadingClass("num",     Num)
  val alphaNumTxt   = TyVar.fromOverloadingClass("numtxt",  NumTxt)

  val tauBool       = InitialStaticEnv.tauBool
  val tauReal       = Type.fromTyVar alphaReal
  val tauRealInt    = Type.fromTyVar alphaRealInt
  val tauWordInt    = Type.fromTyVar alphaWordInt
  val tauNum        = Type.fromTyVar alphaNum
  val tauNumTxt     = Type.fromTyVar alphaNumTxt

  val tauRealInt1   = Type.fromFunType(tauRealInt, tauRealInt)
  val tauNum1       = Type.fromFunType(tauNum, tauNum)
  val tauReal2      = Type.fromFunType(pairType tauReal, tauReal)
  val tauWordInt2   = Type.fromFunType(pairType tauWordInt, tauWordInt)
  val tauNum2       = Type.fromFunType(pairType tauNum, tauNum)
  val tauNumTxt2    = Type.fromFunType(pairType tauNumTxt, tauBool)
  val sigmaRealInt1 = ([alphaRealInt], tauRealInt1)
  val sigmaNum1     = ([alphaNum], tauNum1)
  val sigmaReal2    = ([alphaReal], tauReal2)
  val sigmaWordInt2 = ([alphaWordInt], tauWordInt2)
  val sigmaNum2     = ([alphaNum], tauNum2)
  val sigmaNumTxt2  = ([alphaNumTxt], tauNumTxt2)

  val alpha1        = TyVar.fromInt false 1
  val alpha2        = TyVar.fromInt false 2
  val sigmaUse      = ([alpha1, alpha2],
                        Type.fromFunType(
                          Type.fromTyVar alpha1, Type.fromTyVar alpha2))


  (* Static objects for Modules *)

  (* Static basis *)

  val emptySE = StrIdMap.empty
  val emptyTE = TyConMap.empty
  val emptyVE = VIdMap.empty

  val TE_Word8 : TyEnv = TyConMap.singleton(tyconWord8, (thetaWord8, emptyVE))
  val VE_IO : ValEnv   = VIdMap.singleton(vid_Io, (sigmaIo, e))

  val SE0 : StrEnv =
      StrIdMap.fromList[
        (stridWord8, Env(emptySE, TE_Word8, emptyVE)),
        (stridIO,    Env(emptySE, emptyTE,  VE_IO))
      ]

  val TE0 : TyEnv =
      TyConMap.fromList[
        (tyconVector, (thetaVector, VIdMap.empty))
      ]
                                                                

  val VE0 : ValEnv =
      VIdMap.fromList[
        (vid_Chr,       (sigmaExn, e)),
        (vid_Div,       (sigmaExn, e)),
        (vid_Domain,    (sigmaExn, e)),
        (vid_Overflow,  (sigmaExn, e)),
        (vid_Size,      (sigmaExn, e)),
        (vid_Subscript, (sigmaExn, e)),
        (vidAbs,        (sigmaRealInt1, v)),
        (vidNeg,        (sigmaNum1,     v)),
        (vidPlus,       (sigmaNum2,     v)),
        (vidMinus,      (sigmaNum2,     v)),
        (vidTimes,      (sigmaNum2,     v)),
        (vidDiv,        (sigmaWordInt2, v)),
        (vidMod,        (sigmaWordInt2, v)),
        (vidBy,         (sigmaReal2,    v)),
        (vidLess,       (sigmaNumTxt2,  v)),
        (vidGreater,    (sigmaNumTxt2,  v)),
        (vidLessEq,     (sigmaNumTxt2,  v)),
        (vidGreaterEq,  (sigmaNumTxt2,  v)),
        (vidUse,        (sigmaUse,      v))
      ]

  val E0 = Env(SE0, TE0, VE0)
  val F0 = FunIdMap.empty
  val G0 = SigIdMap.empty
  val T0 = TyNameSet.fromList[tWord8, tVector]

  val B0 = StaticBasis.plus(InitialStaticBasis.B0, (T0, F0, G0, E0))
end;
