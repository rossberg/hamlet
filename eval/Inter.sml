(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML interfaces
 *
 * Definition, Section 7.2
 *)

structure Inter :> INTER =
struct
  (* Import *)

  open IdsCore
  open DynamicObjectsModule


  (* Inheritance *)

  structure GenericEnv =
      GenericEnvFn(
        type Env    = Int
        type ValStr = IdStatus
        type TyStr  = ValInt
        val Env     = Int
        fun unEnv(Int I) = I
      )

  open DynamicObjectsCore


  (* Injections [Section 4.3] *)

  val empty       = GenericEnv.empty

  val fromSI      = GenericEnv.fromSE
  val fromTI      = GenericEnv.fromTE
  val fromVI      = GenericEnv.fromVE
  val fromVIandTI = GenericEnv.fromVEandTE


  (* Projections [Section 4.3] *)

  val SIof = GenericEnv.SEof
  val TIof = GenericEnv.TEof
  val VIof = GenericEnv.VEof


  (* Modification [Section 4.3] *)

  val plus = GenericEnv.plus


  (* Extracting interfaces from environments [Section 7.2] *)

  fun Inter(Env(SE, TE, VE)) = Int(InterSE SE, InterTE TE, InterVE VE)
  and InterVE VE = VIdMap.map (fn(v, is) => is) VE
  and InterTE TE = TyConMap.map InterVE TE
  and InterSE SE = StrIdMap.map Inter SE


  (* Modification [Lookup 4.3] *)

  val findLongTyCon = GenericEnv.findLongTyCon


  (* Cutting down environments [Section 7.2] *)

  fun cutdown(Env(SE, TE, VE), Int(SI, TI, VI)) =
      Env(cutdownSE(SE, SI), cutdownTE(TE, TI), cutdownVE(VE, VI))

  and cutdownVE(VE, VI) =
      VIdMap.foldli
        (fn(vid, is, VE') =>
          case VIdMap.find(VE, vid) of
            SOME(v, is') => VIdMap.insert(VE', vid, (v, is))
          | NONE         => VE'
        ) VIdMap.empty VI

  and cutdownTE(TE, TI) =
      TyConMap.foldli
        (fn(tycon, VI', TE') =>
          case TyConMap.find(TE, tycon) of
            SOME VE' => TyConMap.insert(TE', tycon, cutdownVE(VE', VI'))
          | NONE     => TE'
        ) TyConMap.empty TI

  and cutdownSE(SE, SI) =
      StrIdMap.foldli
        (fn(strid, I, SE') =>
          case StrIdMap.find(SE, strid) of
            SOME E => StrIdMap.insert(SE', strid, cutdown(E, I))
          | NONE   => SE'
        ) StrIdMap.empty SI
end;
