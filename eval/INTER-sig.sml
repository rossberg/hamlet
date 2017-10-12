(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML interfaces
 *
 * Definition, Section 7.2
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

signature INTER =
sig
    (* Import *)

    type longTyCon	= IdsCore.longTyCon
    type longSigId	= IdsModule.longSigId

    type Env		= DynamicObjectsCore.Env
    type Mod		= DynamicObjectsCore.Mod
    type Int		= DynamicObjectsModule.Int
    type ValInt		= DynamicObjectsModule.ValInt
    type TyInt		= DynamicObjectsModule.TyInt
    type StrInt		= DynamicObjectsModule.StrInt
    type SigEnv		= DynamicObjectsModule.SigEnv
    type IntConstraint	= DynamicObjectsModule.IntConstraint


    (* Operations *)

    val empty :		Int

    val fromG :		SigEnv -> Int
    val fromSI :	StrInt -> Int
    val fromTI :	TyInt  -> Int
    val fromVI :	ValInt -> Int
    val fromVIandTI :	ValInt * TyInt -> Int

    val Gof :		Int -> SigEnv
    val SIof :		Int -> StrInt
    val TIof :		Int -> TyInt
    val VIof :		Int -> ValInt

    val plus :		Int * Int -> Int
    val plusSI :	Int * StrInt -> Int
    val plusG :		Int * SigEnv -> Int

    val findLongTyCon :	Int * longTyCon -> ValInt option
    val findLongSigId :	Int * longSigId -> Int option

    val Inter :		Env -> Int
    val cutdown :	Mod * Int -> Mod
    val cutdownIC :	Mod * IntConstraint -> Mod
end;
