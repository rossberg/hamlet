(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of types and type schemes
 *)

structure PPType : PP_TYPE =
struct
  (* Import *)

  open StaticObjectsCore
  open PrettyPrint
  open PPMisc

  infixr ^^ ^/^


  (* Simple objects *)

  fun ppLab lab  = text(Lab.toString lab)
  fun ppTyName t = text(TyName.toString t)

  fun ppOverloadingClass O =
      let
        val T  = OverloadingClass.set O
        val t  = OverloadingClass.default O
        val ts = t :: TyNameSet.listItems(TyNameSet.delete(T, t))
      in
        brack(ppCommaList ppTyName ts)
      end

  fun ppTyVar alpha =
      text(TyVar.toString alpha) ^^
      (case TyVar.overloadingClass alpha of
        NONE   => empty
      | SOME O => ppOverloadingClass O
      )

  fun ppRowVar NONE    = empty
    | ppRowVar(SOME _) = text "," ^/^ text "..."


  (* Precedences *)

  val topPrec   = 0
  val arrowPrec = 1
  val starPrec  = 2
  val applyPrec = 3
  val atomPrec  = 4


  (* Types *)

  fun isTuple([], n)        = n > 2
    | isTuple(lab::labs, n) = lab = Lab.fromInt n andalso isTuple(labs, n+1)

  fun ppType tau              = fbox(below(nest(ppTypePrec topPrec tau)))
  and ppTypePrec p (ref tau') = ppType'Prec p tau'
  and ppType'Prec p (TyVar alpha) =
        ppTyVar alpha
    | ppType'Prec p (RowType(rho, r)) =
      let
        val labtaus = LabMap.listItemsi rho
        val (labs, taus) = ListPair.unzip labtaus
      in
        if not(Option.isSome r) andalso List.null labs then
          text "unit"
        else if not(Option.isSome r) andalso isTuple(labs, 1) then
          parenAt starPrec (p,
            fbox(below(nest(ppStarList (ppTypePrec(starPrec + 1)) taus)))
          )
        else
          brace(ppCommaList ppLabType labtaus ^^ ppRowVar r)
      end
    | ppType'Prec p (FunType(tau1, tau2)) =
        parenAt arrowPrec (p,
          fbox(below(nest(
            ppTypePrec (arrowPrec + 1) tau1 ^/^
            text "->" ^/^
            ppTypePrec arrowPrec tau2
          )))
        )
    | ppType'Prec p (ConsType(taus, t)) =
        fbox(below(nest(ppSeqPrec ppTypePrec applyPrec taus ^/^ ppTyName t)))
    | ppType'Prec p (Undetermined{stamp, eq, ...}) =
        text((if eq then "''" else "'") ^ Stamp.toString stamp)
    | ppType'Prec p (Overloaded O) =
        text "'" ^^ ppOverloadingClass O
    | ppType'Prec p (Determined tau) =
        ppTypePrec p tau

  and ppLabType(lab, tau) =
        fbox(below(nest(hbox(ppLab lab ^/^ text ":") ^/^ ppType tau)))


  (* Type schemes *)

  fun ppTypeScheme sigma =
      let
        val (alphas, tau) = TypeScheme.normalise sigma
      in
        ppType tau
      end
end;
