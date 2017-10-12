(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML modules derived forms
 *
 * Definition, Appendix A
 * + RFC: Fixed manifest type specifications
 * + RFC: Views
 * + RFC: Withtype in signatures
 * + RFC: Abolish sequenced type realisations
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: Local modules
 *
 * Notes:
 * - A phrase named SynDesc has been added to factorize type synonym
 *   specifications.
 * - The structure sharing derived form is missing since it cannot be resolved
 *   syntactically. It has been moved to the bare grammar.
 * - Likewise, the functor signature derived form with a spec argument.
 *)

structure DerivedFormsModule :> DERIVED_FORMS_MODULE =
struct
    (* Import *)

    structure C     = GrammarCore
    structure M     = GrammarModule

    type Info       = M.Info

    type VId        = M.VId
    type TyCon      = M.TyCon
    type StrId      = M.StrId
    type SigId      = M.SigId
    type longTyCon  = M.longTyCon
    type longStrId  = M.longStrId
    type longSigId  = M.longSigId

    type Ty         = C.Ty
    type TyVarseq   = C.TyVarseq
    type TypBind    = C.TypBind
    type Dec        = C.Dec

    type AtStrExp   = M.StrExp
    type AppStrExp  = M.StrExp
    type StrExp     = M.StrExp
    type StrDec     = M.StrDec
    type StrBind    = M.StrBind
    type AtSigExp   = M.SigExp
    type SigExp     = M.SigExp
    type Spec       = M.Spec
    type SynDesc    = M.TypDesc * (M.SigExp -> M.SigExp)
    type DatDesc    = M.DatDesc
    type FunBind    = M.StrBind
    type FunDesc    = M.StrDesc


    (* Rewriting of withtype specifications [Appendix A, 2nd bullet;
     *                                       RFC: Withtype in signatures] *)

    fun rewriteConDesc typbind (M.ConDesc(I, vid, ty_opt, condesc_opt))=
	    M.ConDesc(I, vid,
			 Option.map (DerivedFormsCore.rewriteTy typbind) ty_opt,
			 Option.map (rewriteConDesc typbind) condesc_opt)

    fun rewriteDatDesc typbind (M.DatDesc(I, tyvarseq, tycon, condesc,
							      datdesc_opt)) =
	case DerivedFormsCore.findTyCon(tycon, typbind)
	  of NONE =>
	     M.DatDesc(I, tyvarseq, tycon, rewriteConDesc typbind condesc,
			  Option.map (rewriteDatDesc typbind) datdesc_opt)
	   | SOME _ =>
		Error.error(I, "duplicate type constructor \
			       \in recursive type specification")


    (* Structure Bindings [Figure 18] *)

    fun TRANSStrBind(I, strid, NONE, strexp, strbind_opt) =
	    M.StrBind(I, strid, strexp, strbind_opt)

      | TRANSStrBind(I, strid, SOME sigexp, strexp, strbind_opt) =
	    M.StrBind(I, strid, M.COLONStrExp(I, strexp, sigexp), strbind_opt)

    fun SEALStrBind(I, strid, sigexp, strexp, strbind_opt) =
	    M.StrBind(I, strid, M.SEALStrExp(I, strexp, sigexp), strbind_opt)


    (* Structure Expressions [Figure 18] *)

    (* [RFC: Higher-order functors: RFC: Local modules] *)
    val DECStrExp = M.STRUCTStrExp

    (* [RFC: Higher-order functors] *)
    fun FCTSPECStrExp(I, spec, strexp) =
	let
	    val strid   = StrId.invent()
	    val I'      = M.infoSpec spec
	    val sigexp  = M.SIGSigExp(I', spec)
	    val dec     = C.OPENDec(I', [LongStrId.fromId strid])
	    val strexp' = M.LETStrExp(M.infoStrExp strexp, dec, strexp)
	in
	    M.FCTStrExp(I, strid, sigexp, strexp')
	end


    (* Functor Arguments [Figure 18; Figure 23c; RFC: Higher-order functors] *)

    (* [RFC: Higher-order functors] *)
    datatype FunArg =
	  COLONFunArg of Info * StrId * SigExp
	| SPECFunArg  of Info * Spec


    (* Functor Bindings [Figure 18] *)

    fun buildFCTStrExp(COLONFunArg(I, strid, sigexp), strexp) =
	    M.FCTStrExp(Source.over(I, M.infoStrExp strexp),
			strid, sigexp, strexp)
      | buildFCTStrExp(SPECFunArg(I, spec), strexp) =
	    FCTSPECStrExp(Source.over(I, M.infoStrExp strexp), spec, strexp)

    fun FunBind(I, strid, funargs, strexp, funbind_opt) =
	let
	    val strexp' = List.foldr buildFCTStrExp strexp funargs
	in
	    M.StrBind(I, strid, strexp', funbind_opt)
	end

    (* [RFC: Higher-order functors] *)
    fun TRANSFunBind(I, strid, funargs, NONE, strexp, funbind_opt) =
	    FunBind(I, strid, funargs, strexp, funbind_opt)

      | TRANSFunBind(I, strid, funargs, SOME sigexp', strexp, funbind_opt)=
	    FunBind(I, strid, funargs, M.COLONStrExp(I, strexp,sigexp'),
		       funbind_opt)

    (* [RFC: Higher-order functors] *)
    fun SEALFunBind(I, strid, funargs, sigexp, strexp, funbind_opt) =
	    FunBind(I, strid, funargs, M.SEALStrExp(I, strexp, sigexp),
		       funbind_opt)


    (* Structure Declarations [Figure 18; RFC: Higher-order functors] *)

    (* [RFC: Higher-order functors] *)
    fun FUNCTORStrDec(I, funbind) = M.STRUCTUREStrDec(I, funbind)


    (* Signature Expressions [Figure 19; RFC: Higher-order functors] *)

    (* [RFC: Higher-order functors] *)
    fun SPECSigExp(I, spec) = M.SIGSigExp(I, spec)

    (* [RFC: Higher-order functors] *)
    fun ARROWSigExp(I, sigexp1, sigexp2) =
	    M.FCTSigExp(I, StrId.invent(), sigexp1, sigexp2)

    (* Removed WHERETYPESigExp [RFC: Abolish sequenced type realisations] *)


    (* Functor Descriptions [Figure 19; RFC: Higher-order functors] *)

    (* [RFC: Higher-order functors] *)
    fun FunDesc(I, strid, funargs, sigexp, fundesc_opt) =
	let
	    fun buildFCTSigExp(COLONFunArg(I, strid, sigexp), sigexp') =
		    M.FCTSigExp(Source.over(I, M.infoSigExp sigexp'),
				strid, sigexp, sigexp')
	      | buildFCTSigExp(SPECFunArg(I, spec), sigexp') =
		    M.FCTSPECSigExp(Source.over(I, M.infoSigExp sigexp'),
				    spec, sigexp')
 	    val sigexp' = List.foldr buildFCTSigExp sigexp funargs
	in
	    M.StrDesc(I, strid, sigexp', fundesc_opt)
	end


    (* Specifications [Figure 19] *)

    (* [RFC: Nested signatures] *)
    fun INCLUDEMULTISpec(I,          []           ) = M.EMPTYSpec(I)
      | INCLUDEMULTISpec(I, longsigid::longsigids') =
	let
	    val spec1 = M.INCLUDESpec(I, M.IDSigExp(I, longsigid))
	in
	    M.SEQSpec(I, spec1, INCLUDEMULTISpec(I, longsigids'))
	end

    fun SYNSpec(I, (typdesc, tyrea)) =
	let
	    (* [RFC: Fixed manifest type specifications] *)
	    val sigexp = tyrea(M.SIGSigExp(I, M.TYPESpec(I, typdesc)))
	in
	    M.INCLUDESpec(I, sigexp)
	end

    fun SynDesc(I, tyvarseq, tycon, ty, syndesc_opt) =
	let
	    (* [RFC: Fixed manifest type specifications] *)
	    val (typdesc_opt,tyrea) =
		case syndesc_opt
		  of NONE                 => (NONE, fn sigexp => sigexp)
		   | SOME(typdesc, tyrea) => (SOME typdesc, tyrea)
	in
	    ( M.TypDesc(I, tyvarseq, tycon, typdesc_opt),
	      fn sigexp => tyrea(M.WHERETYPESigExp(I, sigexp, tyvarseq,
						   LongTyCon.fromId tycon, ty)))
	end

    (* [RFC: Withtype in signatures] *)
    fun DATATYPESpec(I, datdesc, NONE)         = M.DATATYPESpec(I, datdesc)
      | DATATYPESpec(I, datdesc, SOME typbind) =
	let
	    fun reinterpretTypBindAsSynDesc(C.TypBind(I, tyvarseq, tycon, ty,
						      typbind_opt)) =
		    SynDesc(I, tyvarseq, tycon, ty,
			    Option.map reinterpretTypBindAsSynDesc typbind_opt)

	    val datdesc' = rewriteDatDesc typbind datdesc
	    val synspec  = reinterpretTypBindAsSynDesc typbind
	in
	    M.SEQSpec(I, M.DATATYPESpec(M.infoDatDesc datdesc, datdesc'),
			 SYNSpec(C.infoTypBind typbind, synspec))
	end

    (* [RFC: Views] *)
    val VIEWTYPE2Spec = M.DATATYPE2Spec

    (* [RFC: Higher-order functors] *)
    fun FUNCTORSpec(I, fundesc) = M.STRUCTURESpec(I, fundesc)
end;
