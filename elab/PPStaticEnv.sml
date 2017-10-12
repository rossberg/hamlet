(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the static environment
 *)

structure PPStaticEnv : PP_STATIC_ENV =
struct
    (* Import *)

    open StaticObjectsCore
    open PrettyPrint
    open PPMisc

    exception Fct = StaticObjectsModule.Fct

    infixr ^^ ^/^


    (* Simple objects *)

    fun ppVId vid     = text(VId.toString vid)
    fun ppTyCon tycon = text(TyCon.toString tycon)
    fun ppTyVar alpha = text(TyVar.toString alpha)
    fun ppStrId strid = text(StrId.toString strid)
    fun ppSigId sigid = text(SigId.toString sigid)

    fun ppTyName t    = text(TyName.toString t)


    (* Precedences *)

    val topPrec   = 0
    val arrowPrec = 1
    val atomPrec  = 2


    (* Environments *)

    fun ppConTypeScheme (_, ref(FunType(tau,_))) =
	    text "of" ^/^ PPType.ppType tau

      | ppConTypeScheme _ = empty


    fun ppValEnv VE =
	VIdMap.foldri
	    (fn(vid, (sigma,IdStatus IdStatus.v), doc) =>
		fbox(nest(
		    hbox(
			text "val" ^/^
			ppVId vid ^/^
			text ":"
		    ) ^/^
		    PPType.ppTypeScheme sigma
		)) ^/^
		doc

	     | (vid, (sigma,_), doc) => doc
	    )
	    empty VE

    fun ppExEnv VE =
	VIdMap.foldri
	    (fn(vid, (sigma,IdStatus IdStatus.e), doc) =>
		fbox(nest(
		    hbox(
			text "exception" ^/^
			ppVId vid
		    ) ^/^
		    ppConTypeScheme sigma
		)) ^/^
		doc

	     | (vid, (sigma,_), doc) => doc
	    )
	    empty VE

    fun ppConEnv VE =
	VIdMap.foldri
	    (fn(vid, (sigma,_), doc) =>
		fbox(nest(
		    ppVId vid ^/^
		    hbox(
			ppConTypeScheme sigma ^/^
			(if isEmpty doc then empty else text "|")
		    )
		)) ^/^
		doc
	    )
	    empty VE


    fun absTy(T, tycon, theta) =
	case TypeFcn.toTyName theta
	  of NONE    => NONE
	   | SOME t  => if TyName.toString t = TyCon.toString tycon
			andalso TyNameSet.member(T, t) then
			    SOME(TyName.admitsEquality t)
			else
			    NONE

    fun ppAbsTyEnv(T,TE) =
	TyConMap.foldri
	    (fn(tycon, (theta as (alphas,tau), VE), doc) =>
		if VIdMap.isEmpty VE then
		case absTy(T, tycon, theta)
		 of NONE    => doc
		  | SOME eq =>
		    fbox(nest(
			hbox(
			    text(if eq then "eqtype" else "type") ^/^
			    ppSeq ppTyVar alphas ^/^
			    ppTyCon tycon
			)
		    )) ^/^
		    doc
		else
		    doc
	    )
	    empty TE

    fun ppSynTyEnv(T,TE) =
	TyConMap.foldri
	    (fn(tycon, (theta as (alphas,tau), VE), doc) =>
		if VIdMap.isEmpty VE
		andalso not(isSome(absTy(T, tycon, theta))) then
		    fbox(nest(
			hbox(
			    text "type" ^/^
			    ppSeq ppTyVar alphas ^/^
			    ppTyCon tycon ^/^
			    text "="
			) ^/^
			PPType.ppType tau
		    )) ^/^
		    doc
		else
		    doc
	    )
	    empty TE

    fun ppDataTyEnv TE =
	TyConMap.foldri
	    (fn(tycon, ((alphas,tau),VE), doc) =>
	        case VIdMap.first VE
		  of SOME(_, IdStatus IdStatus.c) =>
			 fbox(nest(
		             hbox(
				 text "datatype" ^/^
				 ppSeq ppTyVar alphas ^/^
				 ppTyCon tycon ^/^
				 text "="
			     ) ^/^
			    abox(
				ppConEnv VE
			    )
			 )) ^/^
			 doc
		   | SOME(_, TyName _) =>
			 fbox(nest(
		             hbox(
				 text "viewtype" ^/^
				 ppSeq ppTyVar alphas ^/^
				 ppTyCon tycon ^/^
				 text "=" ^/^
				 ppSeq ppTyVar alphas ^/^
				 PPType.ppType tau ^/^
				 text "as"
			     ) ^/^
			    abox(
				ppConEnv VE
			    )
			 )) ^/^
			 doc
		   | _ => doc
	    )
	    empty TE

    fun ppTyEnv(T,TE) =
	    vbox(
		ppAbsTyEnv(T,TE) ^/^
		ppSynTyEnv(T,TE) ^/^
		ppDataTyEnv TE
	    )

    fun ppStrEnv(T,SE) =
	StrIdMap.foldri
	    (fn(strid, M, doc) =>
		fbox(nest(
		    hbox(
			text "structure" ^/^
			ppStrId strid ^/^
			text ":"
		    ) ^/^
		    ppSig' false (T,M)
		)) ^/^
		doc
	    )
	    empty SE

    and ppSigEnv(T,G) =
	SigIdMap.foldri
	    (fn(sigid, StaticObjectsModule.Sig Sigma, doc) =>
		fbox(nest(
		    hbox(
			text "signature" ^/^
			ppSigId sigid ^/^
			text "="
		    ) ^/^
		    ppSig' true Sigma
		)) ^/^
		doc
	      | _ => raise Fail "PPStaticEnv.ppSigEnv: invalid signature"
	    )
	    empty G

    and ppEnv'(T, Env(G,SE,TE,VE)) =
	    vbox(
		ppSigEnv(T,G) ^/^
		ppStrEnv(T,SE) ^/^
		ppTyEnv(T,TE) ^/^
		ppExEnv VE ^/^
		ppValEnv VE
	    )

    and ppEnv E = ppEnv'(TyNameSet.empty, E)


    (* Signatures *)

    and ppTyNameSet T =
	if TyNameSet.isEmpty T then
	    empty
	else
	    comment(ppCommaList ppTyName (TyNameSet.listItems T))

    and ppSig' withT (T,M) = ppSig'Prec withT topPrec (T,M)

    and ppSig'Prec withT p (T, Struct E) =
	    abox(below(
		nest(
		    hbox(
			text "sig" ^/^
			(if withT then ppTyNameSet T else empty)
		    ) ^/^
		    vbox(
			ppEnv'(T, E)
		    )
		) ^/^
		text "end"
	    ))

      (* [RFC: Higher-order functors] *)
      | ppSig'Prec withT p (_, Functor(Fct(T,(M,Sigma)))) =
	let
	    val doc =
		if TyNameSet.isEmpty
		       (TyNameSet.intersection(Sig.tynames Sigma, T)) then
		    fbox(nest(
			ppSig'Prec true (arrowPrec+1) (T,M) ^/^
			text "->" ^/^
			ppSig'Prec true arrowPrec Sigma
		    ))
		else
		    fbox(nest(
			fbox(nest(
			    text "fct " ^^
			    hbox(
				text "Arg" ^/^
				text ":"
			    ) ^/^
			    ppSig'Prec true (arrowPrec+1) (T,M)
			)) ^/^
			text "->" ^/^
			ppSig'Prec true arrowPrec Sigma
		    ))
	in
	    parenAt arrowPrec (p, doc)
	end

      | ppSig'Prec withT p (_, Functor _) =
	    raise Fail "PPStaticEnv.ppSig: invalid functor"

    fun ppSig (T,M) = ppSig' true (T,M)


    (* Recursive export *)

    val _ = PPType.PPStaticEnv.ppSig := ppSig
end;
