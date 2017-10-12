(*
 * (c) Andreas Rossberg 2007
 *
 *)

structure PPGrammar : PP_GRAMMAR =
struct
    open TextIO

    fun ppIndent(out, i) = output(out, CharVector.tabulate(2*i, fn _ => #" "))
    fun ppBegin out      = output(out, "(")
    fun ppEnd out        = output(out, ")")

    fun ppInfo(out, {file, region = ((l1,c1), (l2,c2))}) =
	( case file of NONE   => ()
		     | SOME f => (output(out, f); output(out, ":")) 
	; output(out, Int.toString l1)
	; output(out, ".")
	; output(out, Int.toString c1)
	; output(out, "-")
	; output(out, Int.toString l2)
	; output(out, ".")
	; output(out, Int.toString c2)
	)

    fun ppHead'(out, i, s, I_opt) =
	( ppIndent(out, i)
	; ppBegin out
	; output(out, s)
	; case I_opt of NONE   => ()
		      | SOME I => (output(out, " "); ppInfo(out, I))
	)

    fun ppFoot'(out, i, I_opt) =
	( ppEnd out
	; output(out, "\n")
	)

    fun ppHead(out, i, s, I) = (ppHead'(out, i, s, SOME I); output(out, "\n"))
    fun ppFoot(out, i, I)    = (ppIndent(out, i); ppFoot'(out, i, SOME I))
    fun ppHeadFoot(out, i, s, I) =
	(ppHead'(out, i, s, SOME I); ppFoot'(out, i, SOME I))

    fun ppAtom(out, i, s1, s2) =
	( ppHead'(out, i, s1, NONE)
	; if s2 = "" then () else (output(out, " "); output(out, s2))
	; ppFoot'(out, i, NONE)
	)

    fun ppElem(out, i, s, I, []) =
	  ppHeadFoot(out, i, s, I)
      | ppElem(out, i, s, I, subs) =
	( ppHead(out, i, s, I)
	; List.app (fn pp => pp(out, i+1)) subs
	; ppFoot(out, i, I)
	)

    fun ppOpt ppX (out, i, NONE)    = ()
      | ppOpt ppX (out, i, SOME x) = ppX(out, i, x)

    fun sub ppX x (out, i)      = ppX(out, i, x)
    fun subs ppX xs (out, i)    = List.app (fn x => ppX(out, i, x)) xs
    fun subo ppX x_opt (out, i) = ppOpt ppX (out, i, x_opt)
end;
