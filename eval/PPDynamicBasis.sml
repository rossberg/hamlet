(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of the dynamic basis
 *)

structure PPDynamicBasis : PP_DYNAMIC_BASIS =
struct
  (* Import *)

  type Basis = DynamicObjectsModule.Basis
  type State = DynamicObjectsCore.State

  open PrettyPrint

  infixr ^^ ^/^


  (* Simple objects *)

  fun ppFunId funid = text(FunId.toString funid)
  fun ppSigId sigid = text(SigId.toString sigid)


  (* Environments *)

  fun ppFunEnv F =
      FunIdMap.foldri
        (fn(funid, _, doc) =>
          hbox(text "functor" ^/^ ppFunId funid) ^/^ doc
        ) empty F

  fun ppSigEnv G =
      SigIdMap.foldri
        (fn(sigid, _, doc) =>
          hbox(text "signature" ^/^ ppSigId sigid) ^/^ doc
        ) empty G


  (* Basis *)

  fun ppBasis(s, (F, G, E)) =
      vbox(
        ppSigEnv G ^/^
        ppFunEnv F ^/^
        PPDynamicEnv.ppEnv(s, E) ^/^
        text ""
      )
end;
