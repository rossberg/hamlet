(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML initial static basis
 *
 * Definition, Appendices C and E
 * + RFC: Higher-order functors
 * + RFC: Nested signatures
 *)

structure InitialStaticBasis : INITIAL_STATIC_BASIS =
struct
    (* Import *)

    type Basis = StaticObjectsModule.Basis

    (* Environments [Appendix C; RFC: Higher-order functors;
     *                           RFC: Nested signatures *)

    val T0 = InitialStaticEnv.T0
    val E0 = InitialStaticEnv.E0

    val B0 = (T0,E0)
end;
