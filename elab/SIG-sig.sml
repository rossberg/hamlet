(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML signatures
 *
 * Definition, Sections 5.1, 5.3, and 5.6
 * + RFC: Higher-order functors
 *)

signature SIG =
sig
    (* Import types *)

    type TyVarSet    = StaticObjectsCore.TyVarSet
    type TyNameSet   = StaticObjectsCore.TyNameSet
    type Mod         = StaticObjectsCore.Mod
    type Sig         = StaticObjectsModule.Sig
    type Realisation = Type.Realisation


    (* Operations *)

    val tyvars :	Sig -> TyVarSet
    val tynames :	Sig -> TyNameSet
    val undetermined :	Sig -> bool StampMap.map

    val rename :	Sig -> Sig
    val realise :	Realisation -> Sig -> Sig

    exception Match
    val match :		Mod * Sig -> Mod * Realisation (* Match *)
    val matches :	Sig * Sig -> bool
end;
