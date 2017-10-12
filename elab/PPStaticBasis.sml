(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the static basis
 *)

structure PPStaticBasis : PP_STATIC_BASIS =
struct
    (* Import *)

    type Basis = StaticObjectsModule.Basis

    open PrettyPrint
    open PPMisc

    infixr ^/^


    (* Basis *)

    fun ppBasis (T,E) =
	    (* [RFC: Higher-order modules; RFC: Nested signatures] *)
	    vbox(
		PPStaticEnv.ppTyNameSet T ^/^
		PPStaticEnv.ppEnv E ^/^
		text ""
	    )
end;
