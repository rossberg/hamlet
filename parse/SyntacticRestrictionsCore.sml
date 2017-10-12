(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML syntactic restrictions for the core
 *
 * Definition, Section 2.9
 *
 * Notes: see SYNTACTIC_RESTRICTIONS_CORE-sig.sml
 *)

structure SyntacticRestrictionsCore : SYNTACTIC_RESTRICTIONS_CORE =
struct
  (* Import *)

  open SyntaxCore
  open AnnotationCore
  open BindingObjectsCore
  open Error


  (* Helpers *)

  open BindingContext
  val plus = BindingEnv.plus

  infix plus plusU plusVE plusTE plusVEandTE plusE

  fun ?check(C, NONE)        default = default
    | ?check(C, SOME phrase) default = check(C, phrase)


  (* Checking restriction for vids in binding [Section 2.9, 5th bullet] *)

  fun isValidBindVId vid =
        vid <> VId.fromString "true" andalso
        vid <> VId.fromString "false" andalso
        vid <> VId.fromString "nil" andalso
        vid <> VId.fromString "::"  andalso
        vid <> VId.fromString "ref"

  fun isValidConBindVId vid =
        isValidBindVId vid andalso vid <> VId.fromString "it"


  (* Type variable sequences *)

  fun checkTyVarseq(Seq(tyvars)@@A) =
      (* [Section 2.9, 3rd bullet; Section 3.5, 3rd bullet] *)
      let
        fun check([], U) = U
          | check((tyvar@@A')::tyvars, U) =
              if TyVarSet.member(U, tyvar) then
                errorTyVar(loc A', "duplicate type variable ", tyvar)
              else
                check(tyvars, TyVarSet.add(U, tyvar))
      in
        check(tyvars, TyVarSet.empty)
      end


  (* Atomic Expressions *)

  fun checkAtExp(C, SCONAtExp(scon)@@A) =
        ()
    | checkAtExp(C, IDAtExp(_, longvid)@@A) =
        ()
    | checkAtExp(C, RECORDAtExp(exprow_opt)@@A) =
        ignore(?checkExpRow(C, exprow_opt) LabSet.empty)
    | checkAtExp(C, LETAtExp(dec, exp)@@A) =
      let
        val E = checkDec(C, dec)
      in
        checkExp(C plusE E, exp)
      end
    | checkAtExp(C, PARAtExp(exp)@@A) =
        checkExp(C, exp)


  (* Expression Rows *)

  and checkExpRow(C, ExpRow(lab@@A', exp, exprow_opt)@@A) =
      let
        val ()   = checkExp(C, exp)
        val labs = ?checkExpRow(C, exprow_opt) LabSet.empty
      in
        (* [Section 2.9, 1st bullet] *)
        if LabSet.member(labs, lab) then
          errorLab(loc A', "duplicate label ", lab)
        else
          LabSet.add(labs, lab)
      end


  (* Expressions *)

  and checkExp(C, ATExp(atexp)@@A) =
        checkAtExp(C, atexp)
    | checkExp(C, APPExp(exp, atexp)@@A) =
      ( checkExp(C, exp);
        checkAtExp(C, atexp)
      )
    | checkExp(C, COLONExp(exp, ty)@@A) =
      ( checkExp(C, exp);
        ignore(checkTy(C, ty))
      )
    | checkExp(C, HANDLEExp(exp, match)@@A) =
      ( checkExp(C, exp);
        checkMatch(C, match)
      )
    | checkExp(C, RAISEExp(exp)@@A) =
        checkExp(C, exp)
    | checkExp(C, FNExp(match)@@A) =
        checkMatch(C, match)


  (* Matches *)

  and checkMatch(C, Match(mrule, match_opt)@@A) =
      ( checkMrule(C, mrule);
        ?checkMatch(C, match_opt) ()
      )


  (* Match rules *)

  and checkMrule(C, Mrule(pat, exp)@@A) =
      let
        val VE = checkPat false (C, pat)
      in
        checkExp(C plusVE VE, exp)
      end


  (* Declarations *)

  and checkDec(C, VALDec(tyvarseq, valbind)@@A) =
      let
        val U' = checkTyVarseq(tyvarseq)
        (* Collect implicitly bound tyvars [Section 4.6] *)
        val U =
            TyVarSet.union(
              U',
              TyVarSet.difference(ScopeTyVars.unguardedTyVars valbind, Uof C))
      in
        if not(TyVarSet.isEmpty(TyVarSet.intersection(Uof C, U))) then
          (* [Section 2.9, last bullet] *)
          error(loc A, "some type variables shadow previous ones")
        else
          BindingEnv.fromVE(checkValBind(C plusU U, valbind))
      end
    | checkDec(C, TYPEDec(typbind)@@A) =
        BindingEnv.fromTE(checkTypBind(C, typbind))
    | checkDec(C, DATATYPEDec(datbind)@@A) =
        BindingEnv.fromVEandTE(checkDatBind(C, datbind))
    | checkDec(C, DATATYPE2Dec(tycon@@_, longtycon@@_)@@A) =
      let
         val VE = 
             case findLongTyCon(C, longtycon) of
               SOME VE => VE
             | NONE    => VIdMap.empty  (* is an error later *)
         val TE = TyConMap.singleton(tycon, VE)
      in
        BindingEnv.fromVEandTE(VE,TE)
      end
    | checkDec(C, ABSTYPEDec(datbind, dec)@@A) =
      let
        val (VE, TE) = checkDatBind(C, datbind)
      in
        checkDec(C plusVEandTE (VE, TE), dec)
      end
    | checkDec(C, EXCEPTIONDec(exbind)@@A) =
        BindingEnv.fromVE(checkExBind(C, exbind))
    | checkDec(C, LOCALDec(dec1, dec2)@@A) =
      let
        val E1 = checkDec(C, dec1)
      in
        checkDec(C plusE E1, dec2)
      end
    | checkDec(C, OPENDec(longstrids)@@A) =
      let
        val Es =
            List.map(fn longstrid@@_ =>
              case findLongStrId(C, longstrid) of
                SOME E => E
              | NONE   => BindingEnv.empty  (* is an error later *)
            ) longstrids
      in
        List.foldl (op plus) BindingEnv.empty Es
      end
    | checkDec(C, EMPTYDec@@A) =
        BindingEnv.empty
    | checkDec(C, SEQDec(dec1, dec2)@@A) =
      let
        val E1 = checkDec(C, dec1)
        val E2 = checkDec(C plusE E1, dec2)
      in
        E1 plus E2
      end


  (* Value Bindings *)

  and checkValBind(C, PLAINValBind(pat, exp, valbind_opt)@@A) =
      let
        val VE  = checkPat true (C, pat)
        val ()  = checkExp(C, exp)
        val VE' = ?checkValBind(C, valbind_opt) VIdMap.empty
      in
        VIdMap.appi(fn(vid, _) =>
          if isValidBindVId vid then () else
            (* [Section 2.9, 5th bullet] *)
            errorVId(loc A, "illegal rebinding of identifier ", vid)
        ) VE;
        VIdMap.unionWithi(fn(vid, _, _) =>
          (* [Section 2.9, 2nd bullet] *)
          errorVId(loc A, "duplicate variable ", vid)
        ) (VE, VE')
      end
    | checkValBind(C, RECValBind(valbind)@@A) =
      let
        val VE1 = recValBind(C, valbind)
      in
        checkValBind(C plusVE VE1, valbind)
      end


  (* Type Bindings *)

  and checkTypBind(C, TypBind(tyvarseq, tycon@@A', ty, typbind_opt)@@A) =
      let
        val U1 = checkTyVarseq(tyvarseq)
        val U2 = checkTy(C, ty)
        val TE = ?checkTypBind(C, typbind_opt) TyConMap.empty
      in
        if not(TyVarSet.isSubset(U2, U1)) then
          (* Restriction missing in the Definition! *)
          error(loc(annotation ty), "free type variables in type binding")
        else if TyConMap.inDomain(TE, tycon) then
          (* Syntactic restriction [Section 2.9, 2nd bullet] *)
          errorTyCon(loc A', "duplicate type constructor ", tycon)
        else
          TyConMap.insert(TE, tycon, VIdMap.empty)
      end


  (* Datatype Bindings *)

  and checkDatBind(C, DatBind(tyvarseq, tycon@@A', conbind, datbind_opt)@@A) =
      let
        val U1 = checkTyVarseq(tyvarseq)
        val (U2, VE) = checkConBind(C, conbind)
        val (VE', TE') =
            ?checkDatBind(C, datbind_opt) (VIdMap.empty, TyConMap.empty)
      in
        if not(TyVarSet.isSubset(U2, U1)) then
          (* Restriction missing in Definition! *)
          error(loc(annotation conbind),
            "free type variables in datatype binding")
        else if TyConMap.inDomain(TE', tycon) then
          (* [Section 2.9, 2nd bullet] *)
          errorTyCon(loc A', "duplicate type constructor ", tycon)
        else
          ( VIdMap.unionWithi(fn(vid, _, _) =>
              (* [Section 2.9, 2nd bullet] *)
              errorVId(loc A, "duplicate data constructor ", vid)
            ) (VE, VE'),
            TyConMap.insert(TE', tycon, VE)
          )
      end


  (* Constructor Bindings *)

  and checkConBind(C, ConBind(_, vid@@A', ty_opt, conbind_opt)@@A) =
      let
        val U = ?checkTy(C, ty_opt) TyVarSet.empty
        val (U', VE) =
            ?checkConBind(C, conbind_opt) (TyVarSet.empty, VIdMap.empty)
      in
        if VIdMap.inDomain(VE, vid) then
          (* [Section 2.9, 2nd bullet] *)
          errorVId(loc A', "duplicate data constructor ", vid)
        else if not(isValidConBindVId vid) then
          (* [Section 2.9, 5th bullet] *)
          errorVId(loc A', "illegal rebinding of constructor ", vid)
        else
          (TyVarSet.union(U, U'), VIdMap.insert(VE, vid, IdStatus.c))
      end


  (* Exception Bindings *)

  and checkExBind(C, NEWExBind(_, vid@@A', ty_opt, exbind_opt)@@A) =
      let
        val U  = ?checkTy(C, ty_opt) TyVarSet.empty
        val VE = ?checkExBind(C, exbind_opt) VIdMap.empty
      in
        if VIdMap.inDomain(VE, vid) then
          (* [Section 2.9, 2nd bullet] *)
          errorVId(loc A', "duplicate exception constructor ", vid)
        else if not(isValidConBindVId vid) then
          (* [Section 2.9, 5th bullet] *)
          errorVId(loc A', "illegal rebinding of constructor ", vid)
        else
          VIdMap.insert(VE, vid, IdStatus.e)
      end
    | checkExBind(C, EQUALExBind(_, vid@@A', _, longvid, exbind_opt)@@A) =
      let
        val VE = ?checkExBind(C, exbind_opt) VIdMap.empty
      in
        if VIdMap.inDomain(VE, vid) then
          (* [Section 2.9, 2nd bullet] *)
          errorVId(loc A', "duplicate exception constructor ", vid)
        else
          VIdMap.insert(VE, vid, IdStatus.e)
      end


  (* Atomic Patterns *)

  and checkAtPat inValBind (C, WILDCARDAtPat@@A) =
        VIdMap.empty
    | checkAtPat inValBind (C, SCONAtPat(scon@@_)@@A) =
        (case scon of
          SCon.REAL _ =>
            (* [Section 2.9, 6th bullet] *)
            error(loc A, "real constant in pattern")
        | _ =>
            VIdMap.empty
        )
    | checkAtPat inValBind (C, IDAtPat(_, longvid@@_)@@A) =
      let
        val (strids, vid) = LongVId.explode longvid
      in
        if
          List.null strids andalso
          (case findLongVId(C, longvid) of
            NONE    => true
          | SOME is => is = IdStatus.v
          )
        then
          VIdMap.singleton(vid, IdStatus.v)
        else
          VIdMap.empty
      end
    | checkAtPat inValBind (C, RECORDAtPat(patrow_opt)@@A) =
        #1(?(checkPatRow inValBind)(C, patrow_opt) (VIdMap.empty, LabSet.empty))
    | checkAtPat inValBind (C, PARAtPat(pat)@@A) =
        checkPat inValBind (C, pat)


  (* Pattern Rows *)

  and checkPatRow inValBind (C, DOTSPatRow@@A) =
        (VIdMap.empty, LabSet.empty)
    | checkPatRow inValBind (C, FIELDPatRow(lab@@A', pat, patrow_opt)@@A) =
      let
        val VE = checkPat inValBind (C, pat)
        val (VE', labs) =
            ?(checkPatRow inValBind)(C, patrow_opt) (VIdMap.empty, LabSet.empty)
      in
        if LabSet.member(labs, lab) then
          (* [Section 2.9, 1st bullet] *)
          errorLab(loc A', "duplicate label ", lab)
        else
          ( VIdMap.unionWithi(fn(vid, _, _) =>
              (* [Section 2.9, 2nd bullet] *)
              if inValBind then
                errorVId(loc A, "duplicate variable ", vid)
              else
                IdStatus.v
            ) (VE, VE'),
            LabSet.add(labs, lab)
          )
        end


  (* Patterns *)

  and checkPat inValBind (C, ATPat(atpat)@@A) =
        checkAtPat inValBind (C, atpat)
    | checkPat inValBind (C, CONPat(_, longvid, atpat)@@A) =
        checkAtPat inValBind (C, atpat)
    | checkPat inValBind (C, COLONPat(pat, ty)@@A) =
      ( ignore(checkTy(C, ty));
        checkPat inValBind (C, pat)
      )
    | checkPat inValBind (C, ASPat(_, vid@@A', ty_opt, pat)@@A) =
      let
        val U  = ?checkTy(C, ty_opt) TyVarSet.empty
        val VE = checkPat inValBind (C, pat)
      in
        if inValBind andalso VIdMap.inDomain(VE, vid) then
          (* [Section 2.9, 2nd bullet] *)
          errorVId(loc A', "duplicate variable ", vid)
        else
          VIdMap.insert(VE, vid, IdStatus.v)
      end


  (* Type Expressions *)

  and checkTy(C, VARTy(tyvar@@_)@@A) =
        TyVarSet.singleton tyvar
    | checkTy(C, RECORDTy(tyrow_opt)@@A) =
        #1(?checkTyRow(C, tyrow_opt) (TyVarSet.empty, LabSet.empty))
    | checkTy(C, CONTy(tyseq, longtycon@@_)@@A) =
      let
        val Seq(tys)@@_ = tyseq
        val Us = List.map (fn ty => checkTy(C, ty)) tys
      in
        List.foldl TyVarSet.union TyVarSet.empty Us
      end
    | checkTy(C, ARROWTy(ty, ty')@@A) =
      let
        val U  = checkTy(C, ty)
        val U' = checkTy(C, ty')
      in
        TyVarSet.union(U, U')
      end
    | checkTy(C, PARTy(ty)@@A) =
        checkTy(C, ty)


  (* Type-expression Rows *)

  and checkTyRow(C, TyRow(lab@@A', ty, tyrow_opt)@@A) =
      let
        val U = checkTy(C, ty)
        val (U', labs) =
            ?checkTyRow(C, tyrow_opt) (TyVarSet.empty, LabSet.empty)
      in
        if LabSet.member(labs, lab) then
          (* [Section 2.9, 1st bullet] *)
          errorLab(loc A', "duplicate label ", lab)
        else
          (TyVarSet.union(U, U'), LabSet.add(labs, lab))
      end


  (* Build tentative VE from LHSs of recursive valbind *)

  and recValBind(C, PLAINValBind(pat, exp, valbind_opt)@@A) =
      let
        val VE  = recPat(C, pat)
        val VE' = ?recValBind(C, valbind_opt) VIdMap.empty
      in
        case exp of
          FNExp(_)@@_ =>
            VIdMap.unionWith #2 (VE, VE')
        | _ =>
            (* [Section 2.9, 4th bullet] *)
            error(loc(annotation exp),
              "illegal expression within recursive value binding")
      end
    | recValBind(C, RECValBind(valbind)@@A) =
        recValBind(C, valbind)

  and recPat(C, ATPat(atpat)@@A) =
        recAtPat(C,  atpat)
    | recPat(C, CONPat(_, longvid, atpat)@@A) =
        recAtPat(C, atpat)
    | recPat(C, COLONPat(pat, ty)@@A) =
        recPat(C, pat)
    | recPat(C, ASPat(_, vid@@_, ty_opt, pat)@@A) =
      let
        val VE = recPat(C, pat)
      in
        VIdMap.insert(VE, vid, IdStatus.v)
      end

  and recAtPat(C, WILDCARDAtPat@@A) =
        VIdMap.empty
    | recAtPat(C, SCONAtPat(scon)@@A) =
        VIdMap.empty
    | recAtPat(C, IDAtPat(_, longvid@@_)@@A) =
        (case LongVId.explode longvid of
          ([], vid) => VIdMap.singleton(vid, IdStatus.v)
        | _         => VIdMap.empty
        )
    | recAtPat(C, RECORDAtPat(patrow_opt)@@A) =
        ?recPatRow(C, patrow_opt) VIdMap.empty
    | recAtPat(C, PARAtPat(pat)@@A) =
        recPat(C, pat)

  and recPatRow(C, DOTSPatRow@@A) =
        VIdMap.empty
    | recPatRow(C, FIELDPatRow(lab, pat, patrow_opt)@@A) =
      let
        val VE  = recPat(C, pat)
        val VE' = ?recPatRow(C, patrow_opt) VIdMap.empty
      in
        VIdMap.unionWith #2 (VE, VE')
      end
end;
