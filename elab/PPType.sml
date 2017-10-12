(*
 * (c) Andreas Rossberg 1999-2007
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

    (* Recursive import *)

    structure PPStaticEnv =
    struct
	val ppSig : (TyNameSet * Mod -> PrettyPrint.doc) ref =
 	    ref (fn _ => raise Fail "PPType.PPStaticEnv.ppSig")
    end


    (* Simple objects *)

    fun ppLab lab     = text(Lab.toString lab)
    fun ppTyVar alpha = text(TyVar.toString alpha)
    fun ppTyName t    = text(TyName.toString t)

    fun ppOverloadingClass O =
	let
	    val T  = OverloadingClass.set O
	    val t  = OverloadingClass.default O
	    val ts = t :: TyNameSet.listItems(TyNameSet.delete(T,t))
	in
	    brack(ppCommaList ppTyName ts)
	end


    (* Precedences *)

    val topPrec   = 0
    val arrowPrec = 1
    val starPrec  = 2
    val applyPrec = 3
    val atomPrec  = 4

    (* Types *)

    fun ppType tau = fbox(below(nest(ppTypePrec topPrec tau)))

    and ppTypePrec p (ref tau')        = ppType'Prec p tau'

    and ppType'Prec p (TyVar(alpha))   = ppTyVar alpha

      | ppType'Prec p (RowType rho) =
	let
	    fun isTuple(   [],     n) = n > 2
	      | isTuple(lab::labs, n) =
		    lab = Lab.fromInt n andalso isTuple(labs, n+1)
	in
	    Type.normalizeRow rho;
	    case !rho
	      of FixedRow fixed =>
		 let
		     val labtaus     = LabMap.listItemsi fixed
		     val (labs,taus) = ListPair.unzip labtaus
		 in
		     if List.null labs then
			 text "unit"
		     else if isTuple(labs, 1) then
			 let
			     val doc = fbox(below(nest(
				       ppStarList (ppTypePrec(starPrec+1)) taus
			      )))
			 in
			     parenAt starPrec (p, doc)
			 end
		     else
			 brace(ppCommaList ppLabType labtaus)
		 end
	       | FlexRow{fixed, flex} =>
		 let
		     val labtaus     = LabMap.listItemsi fixed
		     val (labs,taus) = ListPair.unzip labtaus
		 in
		     brace(ppCommaList ppLabType labtaus ^^
			   text "," ^/^ text "...")
		 end
	       | FreeRow _ =>
		     text "{...}"
	end

      | ppType'Prec p (FunType(tau1,tau2)) =
	let
	    val doc = fbox(below(nest(
			  ppTypePrec (arrowPrec+1) tau1 ^/^
			  text "->" ^/^
			  ppTypePrec arrowPrec tau2
		      )))
	in
	    parenAt arrowPrec (p, doc)
	end

      | ppType'Prec p (ConsType(taus,t)) =
	    fbox(below(nest(
		ppSeqPrec ppTypePrec applyPrec taus ^/^ ppTyName t
	    )))

      (* [RFC: First-class modules] *)
      | ppType'Prec p (PackType(StaticObjectsModule.Sig Sigma)) =
	    text "pack " ^^
	    paren(!PPStaticEnv.ppSig Sigma)

      | ppType'Prec p (PackType _) =
	    raise Fail "PPType.ppType: invalid signature"

      | ppType'Prec p (Undetermined{stamp,eq,...}) =
	    text((if eq then "''" else "'") ^ Stamp.toString stamp)

      | ppType'Prec p (Overloaded(O)) =
	    text "'" ^^ ppOverloadingClass O

      | ppType'Prec p (Determined(tau)) =
	    ppTypePrec p tau

    and ppLabType(lab, tau) =
	    fbox(below(nest(
		hbox(
		    ppLab lab ^/^
		    text ":"
		) ^/^
		ppType tau
	    )))


    (* Type schemes *)

    fun ppTypeScheme sigma =
	let
	    val (alphas,tau) = TypeScheme.normalise sigma
	in
	    ppType tau
	end
end;
