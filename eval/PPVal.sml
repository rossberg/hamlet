(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of values
 *)

structure PPVal : PP_VAL =
struct
    (* Import *)

    open DynamicObjectsCore
    open PrettyPrint
    open PPMisc

    infixr ^^ ^/^

    (* Recursive import *)

    structure PPDynamicEnv =
    struct
	val ppMod : (State * Mod -> PrettyPrint.doc) ref =
 	    ref (fn _ => raise Fail "PPVal.PPDynamicEnv.ppMod")
    end

    (* Simple objects *)

    val ppFn        = text "_fn"

    fun ppLab lab   = text(Lab.toString lab)
    fun ppVId vid   = text(VId.toString vid)
    fun ppExName en = text(ExName.toString en)
    fun ppSVal sv   = text(SVal.toString sv)


    (* Precedences *)

    val topPrec   = 0
    val applyPrec = 1
    val atomPrec  = 2


    (* Values *)

    fun ppVal  (s, v) = fbox(below(nest(ppValPrec (topPrec, s) v)))
    and ppExVal(s, e) = fbox(below(nest(ppExValPrec (topPrec, s) e)))

    and ppValPrec (p, s) (Assign) =
	    ppFn

      | ppValPrec (p, s) (SVal sv) =
	    ppSVal sv

      | ppValPrec (p, s) (BasVal b) =
	    ppFn

      | ppValPrec (p, s) (VId vid) =
	    ppVId vid

      | ppValPrec (p, s) (v as VIdVal(vid, v')) =
	(case Val.toList v
	   of SOME vs => brack(ppCommaList (ppValPrec (topPrec, s)) vs)
	    | NONE    =>
	      let
		  val doc = ppVId vid ^/^ ppValPrec (applyPrec+1, s) v'
	      in
		  parenAt applyPrec (p, doc)
	      end
	)

      | ppValPrec (p, s) (ExVal e) =
	    ppExValPrec (p, s) e

      | ppValPrec (p, s) (Record r) =
	let
	    fun isTuple(   [],     n) = n > 2
	      | isTuple(lab::labs, n) =
		    lab = Lab.fromInt n andalso isTuple(labs, n+1)

	    val labvs     = LabMap.listItemsi r
	    val (labs,vs) = ListPair.unzip labvs
	in
	    if List.null labs then
		text "()"
	    else if isTuple(labs, 1) then
		paren(ppCommaList (ppValPrec (topPrec, s)) vs)
	    else
		brace(ppCommaList (ppLabVal s) labvs)
	end

      | ppValPrec (p, s) (Addr a) =
	let
	    val v   = case State.findAddr(s, a)
			of SOME v => v
			 | NONE   => raise Fail "PPVal.ppVal: invalid address"

	    val doc = text "ref" ^/^ ppValPrec (applyPrec+1, s) v
	in
	    parenAt applyPrec (p, doc)
	end

      | ppValPrec (p, s) (FcnClosure _) =
	    ppFn

      (* [RFC: First-class modules] *)
      | ppValPrec (p, s) (Mod M) =
	    text "pack " ^^
	    paren(!PPDynamicEnv.ppMod(s, M))

    and ppLabVal s (lab, v) =
	    abox(nest(
		hbox(
		    ppLab lab ^/^
		    text "="
		) ^/^
		ppVal(s, v)
	    ))


    and ppExValPrec (p, s) (ExName en) =
	    ppExName en

      | ppExValPrec (p, s) (ExNameVal(en, v)) =
	let
	    val doc = ppExName en ^/^ ppValPrec (applyPrec+1, s) v
	in
	    parenAt applyPrec (p, doc)
	end
end;
