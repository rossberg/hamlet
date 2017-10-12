(*
 * (c) Andreas Rossberg 2007
 *
 * Printer for abstract core grammar
 *)

signature PP_CORE =
sig
    (* Import *)

    type VId        = IdsCore.VId
    type TyCon      = IdsCore.TyCon
    type StrId      = IdsCore.StrId
    type longVId    = IdsCore.longVId
    type longTyCon  = IdsCore.longTyCon
    type longStrId  = IdsCore.longStrId
    type TyVarseq   = GrammarCore.TyVarseq
    type AtExp      = GrammarCore.AtExp
    type Ty         = GrammarCore.Ty
    type Dec        = GrammarCore.Dec

    (* Recursive import *)

    structure PPModule :
    sig
        val ppLongSigId :
	    (TextIO.outstream * int * IdsModule.longSigId -> unit) ref
        val ppStrExp :
	    (TextIO.outstream * int * GrammarModule.StrExp -> unit) ref
        val ppStrDec :
	    (TextIO.outstream * int * GrammarCore.StrDec' -> unit) ref
    end

    val ppVId :       TextIO.outstream * int * VId -> unit
    val ppTyCon :     TextIO.outstream * int * TyCon -> unit
    val ppStrId :     TextIO.outstream * int * StrId -> unit
    val ppLongVId :   TextIO.outstream * int * longVId -> unit
    val ppLongTyCon : TextIO.outstream * int * longTyCon -> unit
    val ppLongStrId : TextIO.outstream * int * longStrId -> unit
    val ppTyVarseq :  TextIO.outstream * int * TyVarseq -> unit

    val ppAtExp :     TextIO.outstream * int * AtExp -> unit
    val ppTy :        TextIO.outstream * int * Ty -> unit
    val ppDec :       TextIO.outstream * int * Dec -> unit
end;
