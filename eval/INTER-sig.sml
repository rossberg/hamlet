(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML interfaces
 *
 * Definition, Section 7.2
 *)

signature INTER =
sig
  (* Import *)

  type longTyCon = IdsCore.longTyCon

  type Env    = DynamicObjectsCore.Env
  type Int    = DynamicObjectsModule.Int
  type ValInt = DynamicObjectsModule.ValInt
  type TyInt  = DynamicObjectsModule.TyInt
  type StrInt = DynamicObjectsModule.StrInt


  (* Operations *)

  val empty         : Int

  val fromSI        : StrInt -> Int
  val fromTI        : TyInt  -> Int
  val fromVI        : ValInt -> Int
  val fromVIandTI   : ValInt * TyInt -> Int

  val SIof          : Int -> StrInt
  val TIof          : Int -> TyInt
  val VIof          : Int -> ValInt

  val plus          : Int * Int -> Int

  val findLongTyCon : Int * longTyCon -> ValInt option

  val Inter         : Env -> Int
  val cutdown       : Env * Int -> Env
end;
