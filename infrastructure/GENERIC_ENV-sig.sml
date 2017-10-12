(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML generic core environment
 *
 * Definition, Sections 4.2, 4.3, 6.3 and 7.2
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

signature GENERIC_ENV =
sig
    (* Import *)

    type VId		= IdsCore.VId
    type TyCon		= IdsCore.TyCon
    type StrId		= IdsCore.StrId
    type SigId		= IdsModule.SigId
    type longVId	= IdsCore.longVId
    type longTyCon	= IdsCore.longTyCon
    type longStrId	= IdsCore.longStrId
    type longSigId	= IdsModule.longSigId

    type 'a VIdMap	= 'a IdsCore.VIdMap
    type 'a TyConMap	= 'a IdsCore.TyConMap
    type 'a StrIdMap	= 'a IdsCore.StrIdMap
    type 'a SigIdMap	= 'a IdsModule.SigIdMap


    (* Types [Section 4.2 and 6.3; RFC: Higher-order functors;
     *                             RFC: Nested signatures] *)

    type Env
    type ValStr
    type TyStr
    type ModStr
    type SigStr

    type SigEnv		= SigStr SigIdMap
    type StrEnv		= ModStr StrIdMap
    type TyEnv		= TyStr TyConMap
    type ValEnv		= ValStr VIdMap


    (* Operations [Section 4.3; RFC: Higher-order functors;
     *                          RFC: Nested signatures] *)

    val empty :		Env

    val fromG :		SigEnv -> Env
    val fromSE :	StrEnv -> Env
    val fromTE :	TyEnv  -> Env
    val fromVE :	ValEnv -> Env
    val fromVEandTE :	ValEnv * TyEnv -> Env

    val Gof :		Env -> SigEnv
    val SEof :		Env -> StrEnv
    val TEof :		Env -> TyEnv
    val VEof :		Env -> ValEnv

    val plus :		Env * Env    -> Env
    val plusVE :	Env * ValEnv -> Env
    val plusTE :	Env * TyEnv  -> Env
    val plusVEandTE :	Env * (ValEnv * TyEnv) -> Env
    val plusSE :	Env * StrEnv -> Env
    val plusG :		Env * SigEnv -> Env

    val findVId :	Env * VId       -> ValStr option
    val findTyCon :	Env * TyCon     -> TyStr option
    val findStrId :	Env * StrId     -> ModStr option
    val findSigId :	Env * SigId     -> SigStr option
    val findLongVId :	Env * longVId   -> ValStr option
    val findLongTyCon :	Env * longTyCon -> TyStr option
    val findLongStrId :	Env * longStrId -> ModStr option
    val findLongSigId :	Env * longSigId -> SigStr option

    val disjoint :	Env * Env -> bool
end;
