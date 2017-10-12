(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML modules evaluation
 *
 * Definition, Section 7.3
 *
 * Notes:
 * - See EVAL_MODULE-sig.sml and EvalCore.sml
 * - We only pass the state where necessary, i.e. strexp, strdec, strbind, and
 *   topdec (compare note in [Section 7.3]).
 * - There is a typo in the Definition in rule 182: both occurances of IB
 *   should be replaced by B.
 * - The rules for toplevel declarations are all wrong: in the conclusions,
 *   the result right of the arrow must be B' <+ B''> instead of B'<'> in
 *   all three rules.
 *)

structure EvalModule : EVAL_MODULE =
struct
  (* Import *)

  open SyntaxModule
  open AnnotationModule
  open DynamicObjectsModule
  open Error

  type State = EvalCore.State


  (* Helpers for basis modification *)

  val plus   = DynamicBasis.plus
  val plusSE = DynamicBasis.plusSE
  val plusG  = DynamicBasis.plusG
  val plusF  = DynamicBasis.plusF
  val plusE  = DynamicBasis.plusE

  infix plus plusG plusF plusE plusSE

  fun ?eval(B, NONE)        default = default
    | ?eval(B, SOME phrase) default = eval(B, phrase)


  (* Looking up identifiers *)

  fun findLongStrId(B, longstrid@@A) =
      (case DynamicBasis.findLongStrId(B, longstrid) of
        SOME E => E
      | NONE =>
          errorLongStrId(loc A, "runtime error: unknown structure ", longstrid)
      )

  fun findFunId(B, funid@@A) =
      (case DynamicBasis.findFunId(B, funid) of
        SOME funclosure => funclosure
      | NONE => errorFunId(loc A, "runtime error: unknown functor ", funid)
      )

  fun findLongTyCon(IB, longtycon@@A) =
      (case IntBasis.findLongTyCon(IB, longtycon) of
        SOME I => I
      | NONE => errorLongTyCon(loc A, "runtime error: unknown type ", longtycon)
      )

  fun findSigId(IB, sigid@@A) =
      (case IntBasis.findSigId(IB, sigid) of
        SOME I => I
      | NONE => errorSigId(loc A, "runtime error: unknown signature ", sigid)
      )


  (* Inference rules [Section 7.3] *)

  (* Structure Expressions *)

  fun evalStrExp((s, B), STRUCTStrExp(strdec)@@A) =
      (* [Rule 150] *)
      let
        val E = evalStrDec((s, B), strdec)
      in
        E
      end
    | evalStrExp((s, B), IDStrExp(longstrid)@@A) =
      (* [Rule 151] *)
      let
        val E = findLongStrId(B, longstrid)
      in
        E
      end
    | evalStrExp((s, B), COLONStrExp(strexp, sigexp)@@A) =
      (* [Rule 152] *)
      let
        val E = evalStrExp((s, B), strexp)
        val I = evalSigExp(IntBasis.Inter B, sigexp)
      in
        Inter.cutdown(E, I)
      end
    | evalStrExp((s, B), SEALStrExp(strexp, sigexp)@@A) =
      (* [Rule 153] *)
      let
        val E = evalStrExp((s, B), strexp)
        val I = evalSigExp(IntBasis.Inter B, sigexp)
      in
        Inter.cutdown(E, I)
      end
    | evalStrExp((s, B), APPStrExp(funid, strexp)@@A) =
      (* [Rule 154] *)
      let
        val FunctorClosure((strid, I), strexp', B') = findFunId(B, funid)
        val E  = evalStrExp((s, B), strexp)
        val E' =
            evalStrExp(
              (s, B' plusSE StrIdMap.singleton(strid, Inter.cutdown(E, I))),
              strexp'
            )
      in
        E'
      end
    | evalStrExp((s, B), LETStrExp(strdec, strexp)@@A) =
      (* [Rule 155] *)
      let
        val E  = evalStrDec((s, B), strdec)
        val E' = evalStrExp((s, B plusE E), strexp)
      in
        E'
      end


  (* Structure-level Declarations *)

  and evalStrDec((s, B), DECStrDec(dec)@@A) =
      (* [Rule 156] *)
      let
        val E' = EvalCore.evalDec((s, DynamicBasis.Eof B), dec)
      in
        E'
      end
    | evalStrDec((s, B), STRUCTUREStrDec(strbind)@@A) =
      (* [Rule 157] *)
      let
        val SE = evalStrBind((s, B), strbind)
      in
        DynamicEnv.fromSE SE
      end
    | evalStrDec((s, B), LOCALStrDec(strdec1, strdec2)@@A) =
      (* [Rule 158] *)
      let
        val E1 = evalStrDec((s, B), strdec1)
        val E2 = evalStrDec((s, B plusE E1), strdec2)
      in
        E2
      end
    | evalStrDec((s, B), EMPTYStrDec@@A) =
      (* [Rule 159] *)
      let
      in
        DynamicEnv.empty
      end
    | evalStrDec((s, B), SEQStrDec(strdec1, strdec2)@@A) =
      (* [Rule 160] *)
      let
        val E1 = evalStrDec((s, B), strdec1)
        val E2 = evalStrDec((s, B plusE E1), strdec2)
      in
        DynamicEnv.plus(E1, E2)
      end


  (* Structure Bindings *)

  and evalStrBind((s, B), StrBind(strid@@_, strexp, strbind_opt)@@A) =
      (* [Rule 161] *)
      let
        val E  = evalStrExp((s, B), strexp)
        val SE = ?evalStrBind((s, B), strbind_opt) StrIdMap.empty
      in
        StrIdMap.insert(SE, strid, E)
      end


  (* Signature Expressions *)

  and evalSigExp(IB, SIGSigExp(spec)@@A) =
      (* [Rule 162] *)
      let
        val I = evalSpec(IB, spec)
      in
        I
      end
    | evalSigExp(IB, IDSigExp(sigid)@@A) =
      (* [Rule 163] *)
      let
        val I = findSigId(IB, sigid)
      in
        I
      end
    | evalSigExp(IB, WHERETYPESigExp(sigexp, tyvarseq, longtycon, ty)@@A) =
      (* Omitted [Section 7.1] *)
        evalSigExp(IB, sigexp)


  (* Signature Declarations *)

  and evalSigDec(IB, SigDec(sigbind)@@A) =
      (* [Rule 164] *)
      let
        val G = evalSigBind(IB, sigbind)
      in
        G
      end


  (* Signature Bindings *)

  and evalSigBind(IB, SigBind(sigid@@_, sigexp, sigbind_opt)@@A) =
      (* [Rule 165] *)
      let
        val I = evalSigExp(IB, sigexp)
        val G = ?evalSigBind(IB, sigbind_opt) SigIdMap.empty
      in
        SigIdMap.insert(G, sigid, I)
      end


  (* Specifications *)

  and evalSpec(IB, VALSpec(valdesc)@@A) =
      (* [Rule 166] *)
      let
        val VI = evalValDesc((), valdesc)
      in
        Inter.fromVI VI
      end
    | evalSpec(IB, TYPESpec(typdesc)@@A) =
      (* [Rule 167] *)
      let
        val TI = evalTypDesc((), typdesc)
      in
        Inter.fromTI TI
      end
    | evalSpec(IB, EQTYPESpec(typdesc)@@A) =
      (* [Rule 168] *)
      let
        val TI = evalTypDesc((), typdesc)
      in
        Inter.fromTI TI
      end
    | evalSpec(IB, DATATYPESpec(datdesc)@@A) =
      (* [Rule 169] *)
      let
        val (VI, TI) = evalDatDesc((), datdesc)
      in
        Inter.fromVIandTI(VI, TI)
      end
    | evalSpec(IB, DATATYPE2Spec(tycon@@_, longtycon)@@A) =
      (* [Rule 170] *)
      let
        val VI = findLongTyCon(IB, longtycon)
        val TI = TyConMap.singleton(tycon, VI)
      in
        Inter.fromVIandTI(VI, TI)
      end
    | evalSpec(IB, EXCEPTIONSpec(exdesc)@@A) =
      (* [Rule 171] *)
      let
        val VI = evalExDesc((), exdesc)
      in
        Inter.fromVI VI
      end
    | evalSpec(IB, STRUCTURESpec(strdesc)@@A) =
      (* [Rule 172] *)
      let
        val SI = evalStrDesc(IB, strdesc)
      in
        Inter.fromSI SI
      end
    | evalSpec(IB, INCLUDESpec(sigexp)@@A) =
      (* [Rule 173] *)
      let
        val I = evalSigExp(IB, sigexp)
      in
        I
      end
    | evalSpec(IB, EMPTYSpec@@A) =
      (* [Rule 174] *)
      let
      in
        Inter.empty
      end
    | evalSpec(IB, SEQSpec(spec1, spec2)@@A) =
      (* [Rule 175] *)
      let
        val I1 = evalSpec(IB, spec1)
        val I2 = evalSpec(IntBasis.plusI(IB, I1), spec2)
      in
        Inter.plus(I1, I2)
      end
    | evalSpec(IB, SHARINGTYPESpec(spec, longtycons)@@A) =
      (* Omitted [Section 7.1] *)
        evalSpec(IB, spec)
    | evalSpec(IB, SHARINGSpec(spec, longstrids)@@A) =
      (* Omitted [Section 7.1] *)
        evalSpec(IB, spec)


  (* Value Descriptions *)

  and evalValDesc((), ValDesc(vid@@_, _, valdesc_opt)@@A) =
      (* [Rule 176] *)
      let
        val VI = ?evalValDesc((), valdesc_opt) VIdMap.empty
      in
        VIdMap.insert(VI, vid, IdStatus.v)
      end


  (* Type Descriptions *)

  and evalTypDesc((), TypDesc(tyvarseq, tycon@@_, typdesc_opt)@@A) =
      (* [Rule 177] *)
      let
        val TI = ?evalTypDesc((), typdesc_opt) TyConMap.empty
      in
        TyConMap.insert(TI, tycon, VIdMap.empty)
      end


  (* Datatype Descriptions *)

  and evalDatDesc((), DatDesc(tyvarseq, tycon@@_, condesc, datdesc_opt)@@A) =
      (* [Rule 178] *)
      let
        val  VI        = evalConDesc((), condesc)
        val (VI', TI') =
            ?evalDatDesc((), datdesc_opt) (VIdMap.empty, TyConMap.empty)
      in
        (VIdMap.unionWith #2 (VI, VI'), TyConMap.insert(TI', tycon, VI))
      end


  (* Constructor Descriptions *)

  and evalConDesc((), ConDesc(vid@@_, _, condesc_opt)@@A) =
      (* [Rule 179] *)
      let
        val VI = ?evalConDesc((), condesc_opt) VIdMap.empty
      in
        VIdMap.insert(VI, vid, IdStatus.c)
      end


  (* Exception Description *)

  and evalExDesc((), ExDesc(vid@@_, _, exdesc_opt)@@A) =
      (* [Rule 180] *)
      let
        val VI = ?evalExDesc((), exdesc_opt) VIdMap.empty
      in
        VIdMap.insert(VI, vid, IdStatus.e)
      end


  (* Structure Descriptions *)

  and evalStrDesc(IB, StrDesc(strid@@_, sigexp, strdesc_opt)@@A) =
      (* [Rule 181] *)
      let
        val I  = evalSigExp(IB, sigexp)
        val SI = ?evalStrDesc(IB, strdesc_opt) StrIdMap.empty
      in
        StrIdMap.insert(SI, strid, I)
      end


  (* Functor Bindings *)

  and evalFunBind(B,
        FunBind(funid@@_, strid@@_, sigexp, strexp, funbind_opt)@@A
      ) =
      (* [Rule 182] *)
      (* Note that there is a typo in this rule. *)
      let
        val I = evalSigExp(IntBasis.Inter B, sigexp)
        val F = ?evalFunBind(B, funbind_opt) FunIdMap.empty
      in
        FunIdMap.insert(F, funid, FunctorClosure((strid, I), strexp, B))
      end


  (* Functor Declarations *)

  and evalFunDec(B, FunDec(funbind)@@A) =
      (* [Rule 183] *)
      let
        val F = evalFunBind(B, funbind)
      in
        F
      end


  (* Top-level Declarations *)

  and evalTopDec((s, B), STRDECTopDec(strdec, topdec_opt)@@A) =
      (* [Rule 184] *)
      (* Note the mistake in the conclusion of this rule. *)
      let
        val E   = evalStrDec((s, B), strdec)
        val B'  = DynamicBasis.fromE E
        val B'' = ?evalTopDec((s, B plus B'), topdec_opt) DynamicBasis.empty
      in
        B' plus B''
      end
    | evalTopDec((s, B), SIGDECTopDec(sigdec, topdec_opt)@@A) =
      (* [Rule 185] *)
      (* Note the mistake in the conclusion of this rule. *)
      let
        val G   = evalSigDec(IntBasis.Inter B, sigdec)
        val B'  = DynamicBasis.fromG G
        val B'' = ?evalTopDec((s, B plus B'), topdec_opt) DynamicBasis.empty
      in
        B' plus B''
      end
    | evalTopDec((s, B), FUNDECTopDec(fundec, topdec_opt)@@A) =
      (* [Rule 186] *)
      (* Note the mistake in the conclusion of this rule. *)
      let
        val F   = evalFunDec(B, fundec)
        val B'  = DynamicBasis.fromF F
        val B'' = ?evalTopDec((s, B plus B'), topdec_opt) DynamicBasis.empty
      in
        B' plus B''
      end
end;
