(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML core elaboration
 *
 * Definition, Sections 4.10, 4.11, 4.6, 4.7, 2.9
 * + RFC: Local modules
 * + RFC: First-class modules
 *
 * Notes:
 *   - Elaboration also checks the further restrictions [Section 4.11].
 *   - To implement the 3rd restriction in 4.11 elabDec is passed an
 *     additional boolean argument to recognise being on the toplevel.
 *)

signature ELAB_CORE =
sig
    (* Import *)

    type VId		= IdsCore.VId
    type TyVar		= IdsCore.TyVar

    type AtExp		= GrammarCore.AtExp
    type Dec		= GrammarCore.Dec
    type Ty		= GrammarCore.Ty
    type TyVarseq	= GrammarCore.TyVarseq

    type TyVarSet	= StaticObjectsCore.TyVarSet
    type Type		= StaticObjectsCore.Type
    type Mod		= StaticObjectsCore.Mod
    type Env		= StaticObjectsCore.Env
    type Context	= StaticObjectsCore.Context


    (* Recursive import *)

    structure ElabModule :
    sig
	val elabStrExp : (Context * GrammarModule.StrExp -> Mod) ref
	val elabStrDec : (Context * GrammarCore.StrDec' -> Env) ref
    end


    (* Export *)

    val elabAtExp :	Context * AtExp -> Type
    val elabDec :	bool -> Context * Dec -> Env
    val elabTy :	Context * Ty -> Type
    val tyvars :	TyVarseq -> TyVarSet * TyVar list
end;
