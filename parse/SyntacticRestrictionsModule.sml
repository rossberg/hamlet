(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML syntactic restrictions for modules
 *
 * Definition, Section 3.5
 *)

structure SyntacticRestrictionsModule : SYNTACTIC_RESTRICTIONS_MODULE =
struct
  (* Import *)

  open SyntaxModule
  open AnnotationModule
  open BindingObjectsModule
  open Error


  (* Helpers *)

  val empty  = BindingBasis.plus
  val plus   = BindingBasis.plus
  val plusG  = BindingBasis.plusG
  val plusF  = BindingBasis.plusF
  val plusE  = BindingBasis.plusE
  val plusSE = BindingBasis.plusSE

  infix plus plusG plusF plusE plusSE

  fun ?check(B, NONE)        default = default
    | ?check(B, SOME phrase) default = check(B, phrase)


  (* Structure Expressions *)

  fun checkStrExp(B, STRUCTStrExp(strdec)@@A) =
        checkStrDec(B, strdec)
    | checkStrExp(B, IDStrExp(longstrid@@_)@@A) =
        (case BindingBasis.findLongStrId(B, longstrid) of
          SOME E => E
        | NONE   => BindingEnv.empty  (* is an error later *)
        )
    | checkStrExp(B, COLONStrExp(strexp, sigexp)@@A) =
      ( ignore(checkStrExp(B, strexp));
        checkSigExp(B, sigexp)
      )
    | checkStrExp(B, SEALStrExp(strexp, sigexp)@@A) =
      ( ignore(checkStrExp(B, strexp));
        checkSigExp(B, sigexp)
      )
    | checkStrExp(B, APPStrExp(funid@@_, strexp)@@A) =
      ( ignore(checkStrExp(B, strexp));
        case BindingBasis.findFunId(B, funid) of
          SOME E => E
        | NONE   => BindingEnv.empty  (* is an error later *)
      )
    | checkStrExp(B, LETStrExp(strdec, strexp)@@A) =
      let
        val E1 = checkStrDec(B, strdec)
      in
        checkStrExp(B plusE E1, strexp)
      end


  (* Structure-level Declarations *)

  and checkStrDec(B, DECStrDec(dec)@@A) =
        SyntacticRestrictionsCore.checkDec(BindingBasis.Cof B, dec)
    | checkStrDec(B, STRUCTUREStrDec(strbind)@@A) =
        BindingEnv.fromSE(checkStrBind(B, strbind))
    | checkStrDec(B, LOCALStrDec(strdec1, strdec2)@@A) =
      let
        val E1 = checkStrDec(B, strdec1)
      in
        checkStrDec(B plusE E1, strdec2)
      end
    | checkStrDec(B, EMPTYStrDec@@A) =
        BindingEnv.empty
    | checkStrDec(B, SEQStrDec(strdec1, strdec2)@@A) =
      let
        val E1 = checkStrDec(B, strdec1)
        val E2 = checkStrDec(B plusE E1, strdec2)
      in
        BindingEnv.plus(E1, E2)
      end


  (* Structure Bindings *)

  and checkStrBind(B, StrBind(strid@@A', strexp, strbind_opt)@@A) =
      let
        val E  = checkStrExp(B, strexp)
        val SE = ?checkStrBind(B, strbind_opt) StrIdMap.empty
      in
        if StrIdMap.inDomain(SE, strid) then
          (* [Section 3.5, 1st bullet] *)
          errorStrId(loc A', "duplicate structure identifier ", strid)
        else
          StrIdMap.insert(SE, strid, E)
      end


  (* Signature Expressions *)

  and checkSigExp(B, SIGSigExp(spec)@@A) =
        checkSpec(B, spec)
    | checkSigExp(B, IDSigExp(sigid@@_)@@A) =
        (case BindingBasis.findSigId(B, sigid) of
          SOME E => E
        | NONE   => BindingEnv.empty  (* is an error later *)
        )
    | checkSigExp(B, WHERETYPESigExp(sigexp, tyvarseq, longtycon@@_, ty)@@A) =
      let
        val U1 = SyntacticRestrictionsCore.checkTyVarseq(tyvarseq)
        val U2 = SyntacticRestrictionsCore.checkTy(BindingBasis.Cof B, ty)
      in
        if not(TyVarSet.isSubset(U2, U1)) then
          (* [Section 3.5, 4th bullet] *)
          error(loc(annotation ty), "free type variables in type realisation")
        else
          checkSigExp(B, sigexp)
      end


  (* Signature Declarations *)

  and checkSigDec(B, SigDec(sigbind)@@A) =
        checkSigBind(B, sigbind)


  (* Signature Bindings *)

  and checkSigBind(B, SigBind(sigid@@A', sigexp, sigbind_opt)@@A) =
      let
        val E = checkSigExp(B, sigexp)
        val G = ?checkSigBind(B, sigbind_opt) SigIdMap.empty
      in
        if SigIdMap.inDomain(G, sigid) then
          (* [Section 3.5, 1st bullet] *)
          errorSigId(loc A', "duplicate signature identifier ", sigid)
        else
          SigIdMap.insert(G, sigid, E)
      end


  (* Specifications *)

  and checkSpec(B, VALSpec(valdesc)@@A) =
        BindingEnv.fromVE(checkValDesc(B, valdesc))
    | checkSpec(B, TYPESpec(typdesc)@@A) =
        BindingEnv.fromTE(checkTypDesc(B, typdesc))
    | checkSpec(B, EQTYPESpec(typdesc)@@A) =
        BindingEnv.fromTE(checkTypDesc(B, typdesc))
    | checkSpec(B, DATATYPESpec(datdesc)@@A) =
        BindingEnv.fromVEandTE(checkDatDesc(B, datdesc))
    | checkSpec(B, DATATYPE2Spec(tycon@@_, longtycon@@_)@@A) =
      let
        val VE =
            case BindingBasis.findLongTyCon(B, longtycon) of
              SOME VE => VE
            | NONE    => VIdMap.empty  (* is an error later *)
        val TE = TyConMap.singleton(tycon, VE)
      in
        BindingEnv.fromVEandTE(VE, TE)
      end
    | checkSpec(B, EXCEPTIONSpec(exdesc)@@A) =
        BindingEnv.fromVE(checkExDesc(B, exdesc))
    | checkSpec(B, STRUCTURESpec(strdesc)@@A) =
        BindingEnv.fromSE(checkStrDesc(B, strdesc))
    | checkSpec(B, INCLUDESpec(sigexp)@@A) =
        checkSigExp(B, sigexp)
    | checkSpec(B, EMPTYSpec@@A) =
        BindingEnv.empty
    | checkSpec(B, SEQSpec(spec1, spec2)@@A) =
      let
        val E1 = checkSpec(B, spec1)
        val E2 = checkSpec(B plusE E1, spec2)
      in
        BindingEnv.plus(E1, E2)
      end
    | checkSpec(B, SHARINGTYPESpec(spec, longtycons)@@A) =
        checkSpec(B, spec)
    | checkSpec(B, SHARINGSpec(spec, longstrids)@@A) =
        (* [Appendix A] *)
        checkSpec(B, spec)


  (* Value Descriptions *)

  and checkValDesc(B, ValDesc(vid@@A', ty, valdesc_opt)@@A) =
      let
        val U  = SyntacticRestrictionsCore.checkTy(BindingBasis.Cof B, ty)
        val VE = ?checkValDesc(B, valdesc_opt) VIdMap.empty
      in
        if VIdMap.inDomain(VE, vid) then
          (* [Section 3.5, 2nd bullet] *)
          errorVId(loc A', "duplicate variable ", vid)
        else if not(SyntacticRestrictionsCore.isValidBindVId vid) then
          (* [Section 3.5, 5th bullet] *)
          errorVId(loc A', "illegal specification of identifier ", vid)
        else
          VIdMap.insert(VE, vid, IdStatus.v)
      end


  (* Type Descriptions *)

  and checkTypDesc(B, TypDesc(tyvarseq, tycon@@A', typdesc_opt)@@A) =
      let
        val U  = SyntacticRestrictionsCore.checkTyVarseq(tyvarseq)
        val TE = ?checkTypDesc(B, typdesc_opt) TyConMap.empty
      in
        if TyConMap.inDomain(TE, tycon) then
          (* [Section 3.5, 2nd bullet] *)
          errorTyCon(loc A', "duplicate type constructor ", tycon)
        else
          TyConMap.insert(TE, tycon, VIdMap.empty)
      end


  (* Datatype Descriptions *)

  and checkDatDesc(B, DatDesc(tyvarseq, tycon@@A', condesc, datdesc_opt)@@A) =
      let
        val  U1        = SyntacticRestrictionsCore.checkTyVarseq(tyvarseq)
        val (U2, VE)   = checkConDesc(B, condesc)
        val (VE', TE') =
            ?checkDatDesc(B, datdesc_opt) (VIdMap.empty, TyConMap.empty)
      in
        if TyConMap.inDomain(TE', tycon) then
          (* [Section 3.5, 2nd bullet] *)
          errorTyCon(loc A', "duplicate type constructor ", tycon)
        else if not(TyVarSet.isSubset(U2, U1)) then
          (* [Section 3.5,4th bullet]*)
          error(loc(annotation condesc),
            "free type variables in datatype description")
        else
          ( VIdMap.unionWithi(fn(vid, _, _) =>
              (* [Section 3.5, 2nd bullet] *)
              errorVId(loc A, "duplicate data constructor ", vid)
            ) (VE, VE'),
            TyConMap.insert(TE', tycon, VE)
          )
      end


  (* Constructor Descriptions *)

  and checkConDesc(B, ConDesc(vid@@A', ty_opt, condesc_opt)@@A) =
      let
        val U =
            ?SyntacticRestrictionsCore.checkTy(BindingBasis.Cof B, ty_opt)
              TyVarSet.empty
        val (U', VE) =
            ?checkConDesc(B, condesc_opt) (TyVarSet.empty, VIdMap.empty)
      in
        if VIdMap.inDomain(VE, vid) then
          (* [Section 3.5, 2nd bullet] *)
          errorVId(loc A', "duplicate data constructor ", vid)
        else if not(SyntacticRestrictionsCore.isValidConBindVId vid) then
          (* [Section 3.5, 5th bullet] *)
          errorVId(loc A', "illegal specification of constructor ", vid)
        else
          (TyVarSet.union(U, U'), VIdMap.insert(VE, vid, IdStatus.c))
      end


  (* Exception Description *)

  and checkExDesc(B, ExDesc(vid@@A', ty_opt, exdesc_opt)@@A) =
      let
        val U =
            ?SyntacticRestrictionsCore.checkTy(BindingBasis.Cof B, ty_opt)
              TyVarSet.empty
        val VE = ?checkExDesc(B, exdesc_opt) VIdMap.empty
      in
        if VIdMap.inDomain(VE, vid) then
          (* [Section 3.5, 2nd bullet] *)
          errorVId(loc A', "duplicate exception constructor ", vid)
        else if not(SyntacticRestrictionsCore.isValidConBindVId vid) then
          (* [Section 3.5, 5th bullet] *)
          errorVId(loc A', "illegal specification of constructor ", vid)
        else
          VIdMap.insert(VE, vid, IdStatus.e)
      end


  (* Structure Descriptions *)

  and checkStrDesc(B, StrDesc(strid@@A', sigexp, strdesc_opt)@@A) =
      let
        val E  = checkSigExp(B, sigexp)
        val SE = ?checkStrDesc(B, strdesc_opt) StrIdMap.empty
      in
        if StrIdMap.inDomain(SE, strid) then
          (* [Section 3.5, 2nd bullet] *)
          errorStrId(loc A', "duplicate structure identifier ", strid)
        else
          StrIdMap.insert(SE, strid, E)
      end


  (* Functor Declarations *)

  and checkFunDec(B, FunDec(funbind)@@A) =
        checkFunBind(B, funbind)


  (* Functor Bindings *)

  and checkFunBind(B,
        FunBind(funid@@A', strid@@_, sigexp, strexp, funbind_opt)@@A
      ) =
      let
        val E1 = checkSigExp(B, sigexp)
        val E2 = checkStrExp(B plusSE StrIdMap.singleton(strid, E1), strexp)
        val F  = ?checkFunBind(B, funbind_opt) FunIdMap.empty
      in
        if FunIdMap.inDomain(F, funid) then
          (* [Section 3.5, 1st bullet] *)
          errorFunId(loc A', "duplicate functor identifier ", funid)
        else
          FunIdMap.insert(F, funid, E2)
      end


  (* Top-level Declarations *)

  and checkTopDec(B, STRDECTopDec(strdec, topdec_opt)@@A) =
      let
        val E  = checkStrDec(B, strdec)
        val B' = ?checkTopDec(B plusE E, topdec_opt) BindingBasis.empty
      in
        BindingBasis.fromE E plus B'
      end
    | checkTopDec(B, SIGDECTopDec(sigdec, topdec_opt)@@A) =
      let
        val G  = checkSigDec(B, sigdec)
        val B' = ?checkTopDec(B plusG G, topdec_opt) BindingBasis.empty
      in
        BindingBasis.fromG G plus B'
      end
    | checkTopDec(B, FUNDECTopDec(fundec, topdec_opt)@@A) =
      let
        val F  = checkFunDec(B, fundec)
        val B' = ?checkTopDec(B plusF F, topdec_opt) BindingBasis.empty
      in
        BindingBasis.fromF F plus B'
      end
end;
