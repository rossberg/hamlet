(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML core evaluation
 *
 * Definition, Sections 6.7 and 6.2
 *
 * Notes:
 * - See EVAL_CORE-sig.sml
 * - Rules of the form A |- phrase => A'/p therefore turn into
 *   s, A |- phrase => A'.
 * - We only pass the state where necessary.
 *)

structure EvalCore : EVAL_CORE =
struct
  (* Import *)

  open SyntaxCore
  open AnnotationCore
  open DynamicObjectsCore
  open Error


  (* Helpers for environment modification *)

  val plus        = DynamicEnv.plus
  val plusVE      = DynamicEnv.plusVE
  val plusTE      = DynamicEnv.plusTE
  val plusVEandTE = DynamicEnv.plusVEandTE

  infix plus plusVE plusTE plusVEandTE

  fun ?eval(E, NONE)        default = default
    | ?eval(E, SOME phrase) default = eval(E, phrase)


  (* Evaluating special constants [Section 6.2] *)

  fun valSCon(SCon.INT(b, sc)@@A) =
        SVal(SVal.INT(DynamicLibrary.intFromString(b, sc, try(elab A))))
    | valSCon(SCon.WORD(b, sc)@@A) =
        SVal(SVal.WORD(DynamicLibrary.wordFromString(b, sc, try(elab A))))
    | valSCon(SCon.CHAR(sc)@@A) =
        SVal(SVal.CHAR(DynamicLibrary.charFromString(sc, try(elab A))))
    | valSCon(SCon.STRING(sc)@@A) =
        SVal(SVal.STRING(DynamicLibrary.stringFromString(sc, try(elab A))))
    | valSCon(SCon.REAL(sc)@@A) =
        SVal(SVal.REAL(DynamicLibrary.realFromString(sc, try(elab A))))


  (* Looking up identifiers *)

  fun findLongVId(E, longvid@@A) =
      (case DynamicEnv.findLongVId(E, longvid) of
        SOME valstr => valstr
      | NONE =>
          errorLongVId(loc A, "runtime error: unknown identifier ", longvid)
      )

  fun findLongTyCon(E, longtycon@@A) =
      (case DynamicEnv.findLongTyCon(E, longtycon) of
        SOME tystr => tystr
      | NONE => errorLongTyCon(loc A, "runtime error: unknown type ", longtycon)
      )

  fun findLongStrId(E, longstrid@@A) =
      (case DynamicEnv.findLongStrId(E, longstrid) of
        SOME E => E
      | NONE =>
          errorLongStrId(loc A, "runtime error: unknown structure ", longstrid)
      )


  (* Inference rules [Section 6.7] *)

  (* Atomic Expressions *)

  fun evalAtExp((s, E), SCONAtExp(scon)@@A) =
      (* [Rule 90] *)
      let
      in
        valSCon scon handle Overflow =>
          error(loc A, "runtime error: special constant out of range")
      end
    | evalAtExp((s, E), IDAtExp(_, longvid)@@A) =
      (* [Rule 91] *)
      let
        val (v, is) = findLongVId(E, longvid)
      in
        v
      end
    | evalAtExp((s, E), RECORDAtExp(exprow_opt)@@A) =
      (* [Rule 92] *)
      let
        val r = ?evalExpRow((s, E), exprow_opt) LabMap.empty
      in
        Record r
      end
    | evalAtExp((s, E), LETAtExp(dec, exp)@@A) =
      (* [Rule 93] *)
      let
        val E' = evalDec((s, E), dec)
        val v  = evalExp((s, E plus E'), exp)
      in
        v
      end
    | evalAtExp((s, E), PARAtExp(exp)@@A) =
      (* [Rule 94] *)
      let
        val v = evalExp((s, E), exp)
      in
        v
      end


  (* Expression Rows *)

  and evalExpRow((s, E), ExpRow(lab@@_, exp, exprow_opt)@@A) =
      (* [Rule 95] *)
      let
        val v = evalExp((s, E), exp)
        val r = ?evalExpRow((s, E), exprow_opt) LabMap.empty
      in
        LabMap.insert(r, lab, v)
      end


  (* Expressions *)

  and evalExp((s, E), ATExp(atexp)@@A) =
      (* [Rule 96] *)
      let
        val v = evalAtExp((s, E), atexp)
      in
        v
      end
    | evalExp((s, E), APPExp(exp, atexp)@@A) =
      (* [Rules 97 to 103] *)
      let
        val v_exp = evalExp((s, E), exp)
      in
        case v_exp of
          VId vid =>
            if vid <> VId.fromString "ref" then
              (* [Rule 97] *)
              let
                val v = evalAtExp((s, E), atexp)
              in
                VIdVal(vid, v)
              end
            else
              (* [Rule 99] *)
              let
                val a = Addr.addr()
                val v = evalAtExp((s, E), atexp)
              in
                s := State.insertAddr(!s, a, v);
                Addr a
              end
        | ExVal(ExName en) =>
            (* [Rule 98] *)
            let
              val v = evalAtExp((s, E), atexp)
            in
              ExVal(ExNameVal(en, v))
            end
        | Assign =>
            (* [Rule 100] *)
            let
              val (a, v) =
                  case Val.toPair(evalAtExp((s, E), atexp)) of
                      SOME(Addr a, v) => (a, v)
                    | _ =>
                      error(loc(annotation atexp),
                        "runtime type error: address expected")
            in
              s := State.insertAddr(!s, a, v);
              Record LabMap.empty
            end
        | BasVal b =>
            (* [Rule 101] *)
            let
              val v  = evalAtExp((s, E), atexp)
              val v' = BasVal.APPLY(b, v)
                  handle Pack(e, _) => raise Pack(e, loc A)
                       | BasVal.TypeError s =>
                           error(loc A, "runtime type error: " ^ s)
            in
              v'
            end
        | FcnClosure(match, E', VE) =>
            (* [Rule 102] *)
            (let
              val v  = evalAtExp((s, E), atexp)
              val v' = evalMatch((s, E' plusVE DynamicEnv.Rec VE, v), match)
            in
              v'
            end handle FAIL =>
              (* [Rule 103] *)
              raise Pack(ExName InitialDynamicEnv.enMatch, loc A)
            )
        | _ =>
            error(loc(annotation exp),
              "runtime type error: applicative value expected")
      end
    | evalExp((s, E), COLONExp(exp, ty)@@A) =
      (* Omitted [Section 6.1] *)
        evalExp((s, E), exp)
    | evalExp((s, E), HANDLEExp(exp, match)@@A) =
      (* [Rule 104] *)
      (let
        val v = evalExp((s, E), exp)
      in
        v
      end handle Pack(e, loc) =>
        (* [Rule 105] *)
        let
          val v = evalMatch((s, E, ExVal e), match)
        in
          v
        end handle FAIL =>
          (* [Rule 106] *)
          raise Pack(e, loc)
      )
    | evalExp((s, E), RAISEExp(exp)@@A) =
      (* [Rule 107] *)
      let
        val e =
            case evalExp((s, E), exp) of
              ExVal e => e
            | _ =>
                error(loc(annotation exp),
                  "runtime type error: exception value expected")
      in
        raise Pack(e, loc A)
      end
    | evalExp((s, E), FNExp(match)@@A) =
      (* [Rule 108] *)
      let
      in
        FcnClosure(match, E, VIdMap.empty)
      end


  (* Matches *)

  and evalMatch((s, E, v), Match(mrule, match_opt)@@A) =
      (* [Rule 109] *)
      let
        val v' = evalMrule((s, E, v), mrule)
      in
        v'
      end handle FAIL =>
        case match_opt of
          NONE =>
            (* [Rule 110] *)
              raise FAIL
        | SOME match =>
            (* [Rule 111] *)
            let
              val v' = evalMatch((s, E, v), match)
            in
              v'
            end


  (* Match rules *)

  and evalMrule((s, E, v), Mrule(pat, exp)@@A) =
      (* [Rule 112] *)
      let
        val VE = evalPat((s, E, v), pat)
        val v' = evalExp((s, E plusVE VE), exp)
      in
        v'
      end handle FAIL =>
        (* [Rule 113] *)
        raise FAIL


  (* Declarations *)

  and evalDec((s, E), VALDec(tyvarseq, valbind)@@A) =
      (* [Rule 114] *)
      let
        val VE = evalValBind((s, E), valbind)
      in
        DynamicEnv.fromVE VE
      end
    | evalDec((s, E), TYPEDec(typbind)@@A) =
      (* [Rule 115] *)
      let
        val TE = evalTypBind(s, typbind)
      in
        DynamicEnv.fromTE TE
      end
    | evalDec((s, E), DATATYPEDec(datbind)@@A) =
      (* [Rule 116] *)
      let
        val (VE, TE) = evalDatBind(s, datbind)
      in
        DynamicEnv.fromVEandTE(VE, TE)
      end
    | evalDec((s, E), DATATYPE2Dec(tycon@@_, longtycon)@@A) =
      (* [Rule 117] *)
      let
        val VE = findLongTyCon(E, longtycon)
      in
        DynamicEnv.fromVEandTE(VE, TyConMap.singleton(tycon, VE))
      end
    | evalDec((s, E), ABSTYPEDec(datbind, dec)@@A) =
      (* [Rule 118] *)
      let
        val (VE, TE) = evalDatBind(s, datbind)
        val E'       = evalDec((s, E plusVEandTE (VE, TE)), dec)
      in
        E'
      end
    | evalDec((s, E), EXCEPTIONDec(exbind)@@A) =
      (* [Rule 119] *)
      let
        val VE = evalExBind((s, E), exbind)
      in
        DynamicEnv.fromVE VE
      end
    | evalDec((s, E), LOCALDec(dec1, dec2)@@A) =
      (* [Rule 120] *)
      let
        val E1 = evalDec((s, E), dec1)
        val E2 = evalDec((s, E plus E1), dec2)
      in
        E2
      end
    | evalDec((s, E), OPENDec(longstrids)@@A) =
      (* [Rule 121] *)
      let
        val Es =
            List.map (fn longstrid => findLongStrId(E, longstrid)) longstrids
      in
        List.foldl DynamicEnv.plus DynamicEnv.empty Es
      end
    | evalDec((s, E), EMPTYDec@@A) =
      (* [Rule 122] *)
      let
      in
        DynamicEnv.empty
      end
    | evalDec((s, E), SEQDec(dec1, dec2)@@A) =
      (* [Rule 123] *)
      let
        val E1 = evalDec((s, E), dec1)
        val E2 = evalDec((s, E plus E1), dec2)
      in
        E1 plus E2
      end


  (* Value Bindings *)

  and evalValBind((s, E), PLAINValBind(pat, exp, valbind_opt)@@A) =
      (* [Rule 124] *)
      (let
        val v   = evalExp((s, E), exp)
        val VE  = evalPat((s, E, v), pat)
        val VE' = ?evalValBind((s, E), valbind_opt) VIdMap.empty
      in
        VIdMap.unionWith #2 (VE, VE')
      end handle FAIL =>
        (* [Rule 125] *)
        raise Pack(ExName InitialDynamicEnv.enBind, loc(annotation pat))
      )
    | evalValBind((s, E), RECValBind(valbind)@@A) =
      (* [Rule 126] *)
      let
        val VE = evalValBind((s, E), valbind)
      in
        DynamicEnv.Rec VE
      end


  (* Type Bindings *)

  and evalTypBind(s, TypBind(tyvarseq, tycon@@_, ty, typbind_opt)@@A) =
      (* [Rule 127] *)
      let
        val TE = ?evalTypBind(s, typbind_opt) TyConMap.empty
      in
        TyConMap.insert(TE, tycon, VIdMap.empty)
      end


  (* Datatype Bindings *)

  and evalDatBind(s, DatBind(tyvarseq, tycon@@_, conbind, datbind_opt)@@A) =
      (* [Rule 128] *)
      let
        val VE         = evalConBind(s, conbind)
        val (VE', TE') =
            ?evalDatBind(s, datbind_opt) (VIdMap.empty, TyConMap.empty)
      in
        (VIdMap.unionWith #2 (VE, VE'), TyConMap.insert(TE', tycon, VE))
      end


  (* Constructor Bindings *)

  and evalConBind(s, ConBind(_, vid@@_, _, conbind_opt)@@A) =
      (* [Rule 129] *)
      let
        val VE = ?evalConBind(s, conbind_opt) VIdMap.empty
      in
        VIdMap.insert(VE, vid, (VId vid, IdStatus.c))
      end


  (* Exception Bindings *)

  and evalExBind((s, E), NEWExBind(_, vid@@_, _, exbind_opt)@@A) =
      (* [Rule 130] *)
      let
        val en = ExName.exname vid
        val VE = ?evalExBind((s, E), exbind_opt) VIdMap.empty
      in
        s := State.insertExName(!s, en);
        VIdMap.insert(VE, vid, (ExVal(ExName en), IdStatus.e))
      end
    | evalExBind((s, E), EQUALExBind(_, vid@@_, _, longvid, exbind_opt)@@A) =
      (* [Rule 131] *)
      let
        val en =
            case findLongVId(E, longvid) of
              (en, IdStatus.e) => en
            | _ =>
                errorLongVId(loc(annotation longvid),
                  "runtime error: non-exception identifier ", syntax longvid)
        val VE = ?evalExBind((s, E), exbind_opt) VIdMap.empty
      in
        VIdMap.insert(VE, vid, (en, IdStatus.e))
      end


  (* Atomic Patterns *)

  and evalAtPat((s, E, v), WILDCARDAtPat@@A) =
      (* [Rule 132] *)
      let
      in
        VIdMap.empty
      end
    | evalAtPat((s, E, v), SCONAtPat(scon)@@A) =
      let
        val val_scon = valSCon scon
            handle Overflow =>
              error(loc A, "runtime error: special constant out of range")
      in
        if Val.equal(v, val_scon) then
          (* [Rule 133] *)
          VIdMap.empty
        else
          (* [Rule 134] *)
          raise FAIL
      end
    | evalAtPat((s, E, v), IDAtPat(_, longvid as longvid'@@A')@@A) =
      (* [Rule 135 to 137] *)
      let
        val (strids, vid) = LongVId.explode longvid'
      in
        if
          List.null strids andalso
          (case DynamicEnv.findVId(E, vid) of
            NONE        => true
          | SOME(_, is) => is = IdStatus.v
          )
        then
          (* [Rule 135] *)
          VIdMap.singleton(vid, (v, IdStatus.v))
        else
          let
            val (v', is) = findLongVId(E, longvid)
          in
            if is = IdStatus.v then
              errorLongVId(loc A', "runtime error: non-constructor ", longvid')
            else if Val.equal(v, v') then
              (* [Rule 136] *)
              VIdMap.empty
            else
              (* [Rule 137] *)
              raise FAIL
          end
      end
    | evalAtPat((s, E, v), RECORDAtPat(patrow_opt)@@A) =
      (* [Rule 138] *)
      let
        val r =
            case v of
              Record r => r
            | _ => error(loc A, "runtime type error: record expected")
        val VE = ?evalPatRow((s, E, r), patrow_opt) VIdMap.empty
      in
        if Option.isSome patrow_opt orelse LabMap.isEmpty r then
          VE
        else
          error(loc A, "runtime type error: empty record expected")
      end
    | evalAtPat((s, E, v), PARAtPat(pat)@@A) =
      (* [Rule 139] *)
      let
        val VE = evalPat((s, E, v), pat)
      in
        VE
      end


  (* Pattern Rows *)

  and evalPatRow((s, E, r), DOTSPatRow@@A) =
      (* [Rule 140] *)
      let
      in
        VIdMap.empty
      end
    | evalPatRow((s, E, r), FIELDPatRow(lab@@A', pat, patrow_opt)@@A) =
      (* [Rule 142] *)
      let
        val v =
            case LabMap.find(r, lab) of
              SOME v => v
            | _ =>
                errorLab(loc A',
                  "runtime type error: unmatched label ", lab)
        val VE  = evalPat((s, E, v), pat)
        val VE' = ?evalPatRow((s, E, r), patrow_opt) VIdMap.empty
      in
        VIdMap.unionWithi #2 (VE, VE')
      end handle FAIL =>
        (* [Rule 141] *)
        raise FAIL


  (* Patterns *)

  and evalPat((s, E, v), ATPat(atpat)@@A) =
      (* [Rule 143] *)
      let
        val VE = evalAtPat((s, E, v), atpat)
      in
        VE
      end
    | evalPat((s, E, v), CONPat(_, longvid as longvid'@@A', atpat)@@A) =
      (* [Rules 144 to 148] *)
      let
          val (strids, vid) = LongVId.explode longvid'
      in
        if vid <> VId.fromString "ref" orelse not(List.null strids) then
          case findLongVId(E, longvid) of
            (VId vid, IdStatus.c) =>
              (case
                case v of
                  VIdVal(vid', v') => if vid = vid' then SOME v' else NONE
                | _                => NONE
              of
                SOME v' =>
                  (* [Rule 144] *)
                  let
                    val VE = evalAtPat((s, E, v'), atpat)
                  in
                    VE
                  end
              | NONE =>
                  (* [Rule 145] *)
                  raise FAIL
              )
          | (ExVal(ExName en), IdStatus.e) =>
              (case
                case v of
                  ExVal(ExNameVal(en', v')) =>
                    if en = en' then SOME v' else NONE
                | _ => NONE
              of
                SOME v' =>
                  (* [Rule 146] *)
                  let
                    val VE = evalAtPat((s, E, v'), atpat)
                  in
                    VE
                  end
              | NONE =>
                  (* [Rule 147] *)
                  raise FAIL
              )
          | _ =>
            error(loc A', "runtime type error: constructor expected")
        else
          (* [Rule 148] *)
          let
            val a =
                case v of
                  Addr a => a
                | _ => error(loc A', "runtime type error: address expected")
            val v  = Option.valOf(State.findAddr(!s, a))
            val VE = evalAtPat((s, E, v), atpat)
          in
            VE
          end
      end
    | evalPat((s, E, v), COLONPat(pat, ty)@@A) =
      (* Omitted [Section 6.1] *)
        evalPat((s, E, v), pat)
    | evalPat((s, E, v), ASPat(_, vid@@_, _, pat)@@A) =
      (* [Rule 149] *)
      let
        val VE = evalPat((s, E, v), pat)
      in
        VIdMap.insert(VE, vid, (v, IdStatus.v))
      end
end;
