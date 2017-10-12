(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML interface basis
 *
 * Definition, Section 7.2
 *)

signature INTBASIS =
sig
  (* Import types *)

  type longTyCon = IdsCore.longTyCon
  type SigId     = IdsModule.SigId

  type Int       = DynamicObjectsModule.Int
  type ValInt    = DynamicObjectsModule.ValInt
  type Basis     = DynamicObjectsModule.Basis
  type IntBasis  = DynamicObjectsModule.IntBasis


  (* Operations *)

  val Inter         : Basis -> IntBasis

  val plusI         : IntBasis * Int -> IntBasis

  val findSigId     : IntBasis * SigId     -> Int option
  val findLongTyCon : IntBasis * longTyCon -> ValInt option
end;
