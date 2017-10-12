(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML pretty printing of the dynamic basis
 *)

structure PPDynamicBasis : PP_DYNAMIC_BASIS =
struct
    (* Import *)

    type Basis = DynamicObjectsModule.Basis
    type State = DynamicObjectsCore.State

    open PrettyPrint

    infixr ^/^


    (* Basis *)

    fun ppBasis(s, E) =
	    (* [RFC: Higher-order functors; RFC: Nested signatures] *)
	    vbox(
		PPDynamicEnv.ppEnv(s, E) ^/^
		text ""
	    )
end;
