(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML generic core environment
 *
 * Definition, Sections 4.2, 4.3, 6.3 and 7.2
 *)

signature GENERIC_ENV =
sig
  (* Import *)

  type VId         = IdsCore.VId
  type TyCon       = IdsCore.TyCon
  type StrId       = IdsCore.StrId
  type longVId     = IdsCore.longVId
  type longTyCon   = IdsCore.longTyCon
  type longStrId   = IdsCore.longStrId

  type 'a VIdMap   = 'a IdsCore.VIdMap
  type 'a TyConMap = 'a IdsCore.TyConMap
  type 'a StrIdMap = 'a IdsCore.StrIdMap


  (* Types [Section 4.2 and 6.3] *)

  type ValStr
  type TyStr
  type Env

  type StrEnv = Env StrIdMap
  type TyEnv  = TyStr TyConMap
  type ValEnv = ValStr VIdMap


  (* Operations *)

  val empty        : Env

  val fromSE        : StrEnv -> Env
  val fromTE        : TyEnv  -> Env
  val fromVE        : ValEnv -> Env
  val fromVEandTE   : ValEnv * TyEnv -> Env

  val SEof          : Env -> StrEnv
  val TEof          : Env -> TyEnv
  val VEof          : Env -> ValEnv

  val plus          : Env * Env    -> Env
  val plusVE        : Env * ValEnv -> Env
  val plusTE        : Env * TyEnv  -> Env
  val plusSE        : Env * StrEnv -> Env
  val plusVEandTE   : Env * (ValEnv * TyEnv) -> Env

  val findVId       : Env * VId -> ValStr option
  val findTyCon     : Env * TyCon -> TyStr option
  val findStrId     : Env * StrId -> Env option
  val findLongVId   : Env * longVId -> ValStr option
  val findLongTyCon : Env * longTyCon -> TyStr option
  val findLongStrId : Env * longStrId -> Env option

  val disjoint      : Env * Env -> bool
end;
