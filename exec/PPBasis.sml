(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the combined basis
 *)

structure PPBasis : PP_BASIS =
struct
    (* Import *)

    type Basis = Basis.Basis
    type State = DynamicObjectsCore.State

    open PrettyPrint

    infixr ^^ ^/^


    (* Basis *)

    fun ppBasis (s, ((T,E_STAT), E_DYN)) =
	    (* [RFC: Higher-order functors; RFC: Nested signatures] *)
	    vbox(
		PPEnv.ppEnv(s, (E_STAT,E_DYN)) ^/^
		text ""
	    )
end;
