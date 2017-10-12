(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of types and type schemes
 *)

signature PP_TYPE =
sig
    (* Import *)

    type Type       = StaticObjectsCore.Type
    type TypeScheme = StaticObjectsCore.TypeScheme
    type TyNameSet  = StaticObjectsCore.TyNameSet
    type Mod        = StaticObjectsCore.Mod

    (* Recursive import *)

    structure PPStaticEnv :
	sig val ppSig : (TyNameSet * Mod -> PrettyPrint.doc) ref end

    (* Export *)

    val ppType :	Type -> PrettyPrint.doc
    val ppTypeScheme :	TypeScheme -> PrettyPrint.doc
end;
