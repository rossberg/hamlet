(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML modules evaluation
 *
 * Definition, Section 7.3
 * + RFC: Semantic fixes
 * + RFC: Views
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 * + RFC: First-class modules
 *
 * Notes:
 *   - State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     can be avoided (would really be a pain). Note that the state therefore
 *     never is returned.
 *   - Doing so, we can model the exception convention using exceptions.
 *     Rules of the form A |- phrase => A'/p therefore turn into
 *     A |- phrase => A'.
 *   - We only pass the state where necessary, ie. strexp, strdec, strbind, and
 *     topdec (compare note in [Section 7.3]).
 *)

structure EvalModule : EVAL_MODULE =
struct
    (* Import *)

    open GrammarModule
    open DynamicObjectsCore
    open DynamicObjectsModule
    open Error

    type State = EvalCore.State


    (* Helpers for basis modification *)

    val plus    = DynamicBasis.plus
    val plusSE  = DynamicBasis.plusSE
    val plusG   = DynamicBasis.plusG
    val plusE   = DynamicBasis.plusE

    infix plus plusG plusE plusSE



    (* Inference rules [Section 7.3] *)


    (* Structure Expressions *)

    fun evalStrExp(s,B, STRUCTStrExp(I, dec)) =
	(* [Rule 150; RFC: Higher-order functors; RFC: Local modules] *)
	let
	    val E = EvalCore.evalDec(s,DynamicBasis.Eof B, dec)
	in
	    Struct E
	end

      | evalStrExp(s,B, IDStrExp(I, longstrid)) =
	(* [Rule 151] *)
	let
	    val M = case DynamicBasis.findLongStrId(B, longstrid)
		      of SOME M => M
		       | NONE =>
			 errorLongStrId(I, "runtime error: unknown structure ",
					   longstrid)
	in
	    M
	end

      | evalStrExp(s,B, COLONStrExp(I, strexp, sigexp)) =
	(* [Rule 152] *)
	let
	    val M = evalStrExp(s,B, strexp)
	    val I = evalSigExp(Inter.Inter B, sigexp)
	in
	    Inter.cutdown(M, I)
	end

      | evalStrExp(s,B, SEALStrExp(_, strexp, sigexp)) =
	(* [Rule 153] *)
	let
	    val M = evalStrExp(s,B, strexp)
	    val I = evalSigExp(Inter.Inter B, sigexp)
	in
	    Inter.cutdown(M, I)
	end

      | evalStrExp(s,B, UNPACKStrExp(I', atexp, sigexp)) =
	(* [Rule 153a; RFC: First-class modules] *)
	let
	    val v = EvalCore.evalAtExp(s,DynamicBasis.Eof B, atexp)
	    val I = evalSigExp(Inter.Inter B, sigexp)
	in
	    case v
	      of DynamicObjectsCore.Mod M =>
		    Inter.cutdown(M, I)
	       | _ => error(I', "runtime type error: package expected")
	end

      | evalStrExp(s,B, APPStrExp(I', strexp1, strexp2)) =
	(* [Rule 154; RFC: Higher-order functors] *)
	let
	    val M1 = evalStrExp(s,B, strexp1)
	    val M  = evalStrExp(s,B, strexp2)
	in
	    case M1
	      of Functor(Fct((strid,I), (strexp',IC), B')) =>
		 let
		    val M' = evalStrExp(s,
					B' plusSE
					StrIdMap.singleton(strid,
							   Inter.cutdown(M,I)),
					strexp')
		 in
		    Inter.cutdownIC(M', IC)
		 end
	       | _ => error(I', "runtime type error: functor expected")
	end

      | evalStrExp(s,B, LETStrExp(I, dec, strexp)) =
	(* [Rule 155; RFC: Local modules] *)
	let
	    val E = EvalCore.evalDec(s,DynamicBasis.Eof B, dec)
	    val M = evalStrExp(s,B plusE E, strexp)
	in
	    M
	end

      | evalStrExp(s,B, FCTStrExp(I', strid, sigexp, strexp)) =
	(* [Rule 155a; RFC: Higher-order functors] *)
	let
	    val I = evalSigExp(Inter.Inter B, sigexp)
	in
	    Functor(Fct((strid,I), (strexp,NONE), B))
	end

      | evalStrExp(s,B, PARStrExp(I, strexp)) =
	(* [Rule 155b; RFC: Higher-order functors] *)
	let
	    val M = evalStrExp(s,B, strexp)
	in
	    M
	end


    (* Structure-level Declarations *)

	(* Removed rule 156 [RFC: Local modules] *)

    and evalStrDec(s,B, STRUCTUREStrDec(I, strbind)) =
	(* [Rule 157] *)
	let
	    val SE = evalStrBind(s,B, strbind)
	in
	    DynamicEnv.fromSE SE
	end

      | evalStrDec(s,B, SIGNATUREStrDec(I, sigbind)) =
	(* [Rule 157a; RFC: Nested signatures] *)
	let
	    val G = evalSigBind(Inter.Inter B, sigbind)
	in
	    DynamicEnv.fromG G
	end

	(* Removed rule 158 [RFC: Local modules] *)

	(* Removed rule 159 [RFC: Local modules] *)

	(* Removed rule 160 [RFC: Local modules] *)


    (* Structure Bindings *)

    and evalStrBind(s,B, StrBind(I, strid, strexp, strbind_opt)) =
	(* [Rule 161] *)
	let
	    val E  = evalStrExp(s,B, strexp)
	    val SE = case strbind_opt
		       of NONE         => StrIdMap.empty
		        | SOME strbind => evalStrBind(s,B, strbind)
	in
	    StrIdMap.insert(SE, strid, E)
	end


    (* Signature Expressions *)

    and evalSigExp(I, SIGSigExp(_, spec)) =
	(* [Rule 162; RFC: Nested signatures] *)
	let
	    val I' = evalSpec(I, spec)
	in
	    I'
	end

      | evalSigExp(I, IDSigExp(I'', longsigid)) =
	(* [Rule 163; RFC: Nested signatures] *)
	let
	    val I' = case Inter.findLongSigId(I, longsigid)
		       of SOME I' => I'
			| NONE    => errorLongSigId(I'', "runtime error: unknown \
							 \signature ", longsigid)
	in
	    I'
	end

      | evalSigExp(I, WHERETYPESigExp(_, sigexp, _, _, _)) =
	(* Omitted [Section 7.1] *)
	evalSigExp(I, sigexp)

      | evalSigExp(I, FCTSigExp(_, strid, sigexp1, sigexp2)) =
	(* [Rule 163a; RFC: Higher-order functors; RFC: Nested signatures] *)
	let
	    val I1 = evalSigExp(I, sigexp1)
	    val I2 = evalSigExp(Inter.plusSI(I, StrIdMap.singleton(strid, I1)),
				sigexp2)
	in
	    I2
	end

      | evalSigExp(I, FCTSPECSigExp(_, spec, sigexp2)) =
	(* [Appendix A; RFC: Higher-order functors; RFC: Nested signatures] *)
	let
	    val I1 = evalSpec(I, spec)
	    val I2 = evalSigExp(Inter.plus(I, I1), sigexp2)
	in
	    I2
	end

      | evalSigExp(I, PARSigExp(_, sigexp)) =
	(* [Rule 163b; RFC: Higher-order functors; RFC: Nested signatures] *)
	let
	    val I' = evalSigExp(I, sigexp)
	in
	    I'
	end


    (* Removed rule 164 [RFC: Nested signatures] *)


    (* Signature Bindings *)

    and evalSigBind(I, SigBind(_, sigid, sigexp, sigbind_opt)) =
	(* [Rule 165; RFC: Nested signatures] *)
	let
	    val I' = evalSigExp(I, sigexp)
	    val G  = case sigbind_opt
		       of NONE         => SigIdMap.empty
			| SOME sigbind => evalSigBind(I, sigbind)
	in
	    SigIdMap.insert(G, sigid, Sig I')
	end


    (* Specifications *)

    and evalSpec(I, VALSpec(_, valdesc)) =
	(* [Rule 166; RFC: Nested signatures] *)
	let
	    val VI = evalValDesc(valdesc)
	in
	    Inter.fromVI VI
	end

      | evalSpec(I, TYPESpec(_, typdesc)) =
	(* [Rule 167; RFC: Nested signatures] *)
	let
	    val TI = evalTypDesc(typdesc)
	in
	    Inter.fromTI TI
	end

      | evalSpec(I, EQTYPESpec(_, typdesc)) =
	(* [Rule 168; RFC: Nested signatures] *)
	let
	    val TI = evalTypDesc(typdesc)
	in
	    Inter.fromTI TI
	end

      | evalSpec(I, DATATYPESpec(_, datdesc)) =
	(* [Rule 169; RFC: Nested signatures] *)
	let
	    val (VI,TI) = evalDatDesc(datdesc)
	in
	    Inter.fromVIandTI(VI,TI)
	end

      | evalSpec(I, VIEWTYPESpec(I', tyvarseq, tycon, _, condesc)) =
	(* [Rule 169a; RFC: Views] *)
	let
	    val VI  = evalConDesc condesc
	    val VI' = VIdMap.map (fn c => f) VI
	    val TI  = TyConMap.singleton(tycon, VI')
	in
	    Inter.fromVIandTI(VI', TI)
	end

      | evalSpec(I, DATATYPE2Spec(I', tycon, longtycon)) =
	(* [Rule 170; RFC: Nested signatures] *)
	let
	    val VI = case Inter.findLongTyCon(I, longtycon)
		       of SOME VI => VI
			| NONE => errorLongTyCon(I', "runtime error: \
						     \unknown type ", longtycon)
	    val TI = TyConMap.singleton(tycon, VI)
	in
	    Inter.fromVIandTI(VI,TI)
	end

      | evalSpec(I, EXCEPTIONSpec(_, exdesc)) =
	(* [Rule 171; RFC: Nested signatures] *)
	let
	    val VI = evalExDesc(exdesc)
	in
	    Inter.fromVI VI
	end

      | evalSpec(I, STRUCTURESpec(_, strdesc)) =
	(* [Rule 172; RFC: Nested signatures] *)
	let
	    val SI = evalStrDesc(I, strdesc)
	in
	    Inter.fromSI SI
	end

      | evalSpec(I, SIGNATURESpec(_, sigdesc)) =
	(* [Rule 172a; RFC: Nested signatures] *)
	let
	    val G = evalSigDesc(I, sigdesc)
	in
	    Inter.fromG G
	end

      | evalSpec(I, INCLUDESpec(_, sigexp)) =
	(* [Rule 173; RFC: Nested signatures] *)
	let
	    val I' = evalSigExp(I, sigexp)
	in
	    I'
	end

      | evalSpec(I, EMPTYSpec(_)) =
	(* [Rule 174; RFC: Nested signatures] *)
	Inter.empty

      | evalSpec(I, SEQSpec(I', spec1, spec2)) =
	(* [Rule 175; RFC: Nested signatures] *)
	let
	    val I1 = evalSpec(I, spec1)
	    val I2 = evalSpec(Inter.plus(I, I1), spec2)
	in
	    Inter.plus(I1,I2)
	end

      | evalSpec(I, SHARINGTYPESpec(_, spec, longtycons)) =
	(* Omitted [Section 7.1] *)
	evalSpec(I, spec)

      | evalSpec(I, SHARINGSpec(_, spec, longstrids)) =
	(* Omitted [Section 7.1] *)
	evalSpec(I, spec)


    (* Value Descriptions *)

    and evalValDesc(ValDesc(I, vid, _, valdesc_opt)) =
	(* [Rule 176] *)
	let
	    val VI = case valdesc_opt
		       of NONE         => VIdMap.empty
			| SOME valdesc => evalValDesc(valdesc)
	in
	    VIdMap.insert(VI, vid, IdStatus IdStatus.v)
	end


    (* Type Descriptions *)

    and evalTypDesc(TypDesc(I, tyvarseq, tycon, typdesc_opt)) =
	(* [Rule 177] *)
	let
	    val TI = case typdesc_opt
		       of NONE         => TyConMap.empty
			| SOME typdesc => evalTypDesc(typdesc)
	in
	    TyConMap.insert(TI, tycon, VIdMap.empty)
	end


    (* Datatype Descriptions *)

    and evalDatDesc(DatDesc(I, tyvarseq, tycon, condesc, datdesc_opt)) =
	(* [Rule 178] *)
	let
	    val  VI       = evalConDesc(condesc)
	    val (VI',TI') = case datdesc_opt
			      of NONE          => ( VIdMap.empty, TyConMap.empty )
			       | SOME datdesc' => evalDatDesc(datdesc')
	in
	    ( VIdMap.unionWith #2 (VI, VI')
	    , TyConMap.insert(TI', tycon, VI)
	    )
	end


    (* Constructor Descriptions *)

    and evalConDesc(ConDesc(I, vid, _, condesc_opt)) =
	(* [Rule 179] *)
	let
	    val VI = case condesc_opt
		       of NONE         => VIdMap.empty
			| SOME condesc => evalConDesc(condesc)
	in
	    VIdMap.insert(VI, vid, IdStatus IdStatus.c)
	end


    (* Exception Description *)

    and evalExDesc(ExDesc(I, vid, _, exdesc_opt)) =
	(* [Rule 180] *)
	let
	    val VI = case exdesc_opt
		       of NONE        => VIdMap.empty
			| SOME exdesc => evalExDesc(exdesc)
	in
	    VIdMap.insert(VI, vid, IdStatus IdStatus.e)
	end


    (* Structure Descriptions *)

    and evalStrDesc(I, StrDesc(_, strid, sigexp, strdesc_opt)) =
	(* [Rule 181; RFC: Nested signatures] *)
	let
	    val I' = evalSigExp(I, sigexp)
	    val SI = case strdesc_opt
		       of NONE         => StrIdMap.empty
		        | SOME strdesc => evalStrDesc(I, strdesc)
	in
	    StrIdMap.insert(SI, strid, I')
	end


    (* Signature Descriptions *)

    and evalSigDesc(I, SigDesc(_, sigid, sigexp, sigdesc_opt)) =
	(* [Rule 181a; RFC: Nested signatures] *)
	let
	    val I' = evalSigExp(I, sigexp)
	    val G  = case sigdesc_opt
		       of NONE         => SigIdMap.empty
		        | SOME sigdesc => evalSigDesc(I, sigdesc)
	in
	    SigIdMap.insert(G, sigid, Sig I')
	end


    (* Removed rule 182 [RFC: Higher-order functors] *)

    (* Removed rule 183 [RFC: Higher-order functors] *)


    (* Top-level Declarations *)

    and evalTopDec(s,B, TopDec(I, dec)) =
	(* [Rule 184; RFC: Semantic fixes; RFC: Nested signatures;
	 *                                 RFC: Local modules] *)
	let
	    val E = EvalCore.evalDec(s,DynamicBasis.Eof B, dec)
	in
	    DynamicBasis.fromE E
	end

      (* Removed rule 185 [RFC: Nested signatures] *)

      (* Removed rule 186 [RFC: Higher-order functors] *)


    (* Tie recursive imports *)

    val _ = EvalCore.EvalModule.evalStrExp := evalStrExp
    val _ = EvalCore.EvalModule.evalStrDec :=
	    (fn (s,E, StrDec strdec) => evalStrDec(s,E, strdec)
	      | _ => raise Fail "EvalModule.evalStrDec: invalid declaration")
end;
