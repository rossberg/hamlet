(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the dynamic environment
 *)

structure PPDynamicEnv : PP_DYNAMIC_ENV =
struct
    (* Import *)

    open DynamicObjectsCore
    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^


    (* Simple objects *)

    val ppFct         = text "_fct"

    fun ppVId vid     = text(VId.toString vid)
    fun ppTyCon tycon = text(TyCon.toString tycon)
    fun ppStrId strid = text(StrId.toString strid)
    fun ppSigId sigid = text(SigId.toString sigid)


    (* Environments *)

    fun ppValEnv(s, VE) =
	VIdMap.foldri
	    (fn(vid, (v,IdStatus IdStatus.v), doc) =>
		fbox(nest(
		    hbox(
			text "val" ^/^
			ppVId vid ^/^
			text "="
		    ) ^/^
		    PPVal.ppVal(s, v)
		)) ^/^
		doc

	     | (vid, (v,_), doc) => doc
	    )
	    empty VE

    fun ppExEnv VE =
	VIdMap.foldri
	    (fn(vid, (v,IdStatus IdStatus.e), doc) =>
		hbox(
		    text "exception" ^/^
		    ppVId vid
		) ^/^
		doc

	     | (vid, (v,_), doc) => doc
	    )
	    empty VE

    fun ppConEnv VE =
	VIdMap.foldli
	    (fn(vid, (v,IdStatus IdStatus.c), doc) =>
		hbox(
		    text "con" ^/^
		    ppVId vid
		) ^/^
		doc

	     | (vid, (v,Vals _), doc) =>
		hbox(
		    text "con" ^/^
		    ppVId vid
		) ^/^
		doc

	     | (vid, (v,_), doc) => doc
	    )
	    empty VE

    fun ppTyEnv(s, TE) =
	TyConMap.foldri
	    (fn(tycon, VE, doc) =>
		fbox(nest(
		    hbox(
			text "type" ^/^
			ppTyCon tycon
		    )
		)) ^/^
		doc
	    )
	    empty TE

    fun ppStrEnv(s, SE) =
	StrIdMap.foldri
	    (fn(strid, M, doc) =>
		fbox(nest(
		    hbox(
			text "structure" ^/^
			ppStrId strid ^/^
			text "="
		    ) ^/^
		    ppMod(s, M)
		)) ^/^
		doc
	    )
	    empty SE

    (* [RFC: Nested signatures] *)
    and ppSigEnv(s, G) =
	SigIdMap.foldri
	    (fn(sigid, I, doc) =>
		fbox(nest(
		    hbox(
			text "signature" ^/^
			ppSigId sigid
		    )
		)) ^/^
		doc
	    )
	    empty G

    and ppEnv(s, DynamicObjectsCore.Env(G,SE,TE,VE)) =
	    vbox(
		(* [RFC: Nested signatures] *)
		ppSigEnv(s, G) ^/^
		ppStrEnv(s, SE) ^/^
		ppTyEnv(s, TE) ^/^
		ppConEnv VE ^/^
		ppExEnv  VE ^/^
		ppValEnv(s, VE)
	    )


    (* Structures *)

    and ppMod(s, DynamicObjectsCore.Struct E) =
	    abox(below(
		nest(
		    text "struct" ^/^
		    vbox(
			ppEnv(s, E)
		    )
		) ^/^
		text "end"
	    ))
    
      (* [RFC: Higher-order functors] *)
      | ppMod(s, DynamicObjectsCore.Functor _) =
	    ppFct


    (* Recursive export *)

    val _ = PPVal.PPDynamicEnv.ppMod := ppMod
end;
