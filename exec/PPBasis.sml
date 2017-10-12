(*
 * (c) Andreas Rossberg 1999-2013
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

  fun ppBasis (s, ((T, F_STAT, G_STAT, E_STAT), (F_DYN, G_DYN, E_DYN))) =
      vbox(
        PPStaticBasis.ppSigEnv G_STAT ^/^
        PPStaticBasis.ppFunEnv F_STAT ^/^
        PPEnv.ppEnv(s, (E_STAT, E_DYN)) ^/^
        text ""
      )
end;
