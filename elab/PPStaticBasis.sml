(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of the static basis
 *)

structure PPStaticBasis : PP_STATIC_BASIS =
struct
  (* Import *)

  type Basis  = StaticObjectsModule.Basis
  type SigEnv = StaticObjectsModule.SigEnv
  type FunEnv = StaticObjectsModule.FunEnv

  open PrettyPrint
  open PPMisc

  infixr ^^ ^/^


  (* Simple objects *)

  fun ppSigId sigid = text(SigId.toString sigid)
  fun ppFunId funid = text(FunId.toString funid)


  (* Environments *)

  fun ppSigEnv G =
      SigIdMap.foldri
        (fn(sigid, Sigma, doc) =>
          fbox(nest(
            hbox(text "signature" ^/^ ppSigId sigid ^/^ text "=") ^/^
              PPStaticEnv.ppSig Sigma
          )) ^/^ doc
        ) empty G

  fun ppFunEnv F =
      FunIdMap.foldri
        (fn(funid, (T, (E, Sigma)), doc) =>
          fbox(nest(
            hbox(text "functor" ^/^ ppFunId funid) ^^ ebreak ^^
            fbox(nest(
              text "(" ^^
                hbox(text "Arg" ^/^ text ":") ^/^ PPStaticEnv.ppSig(T, E) ^^
              text ")"
            )) ^/^ text ":" ^/^ PPStaticEnv.ppSig Sigma
          )) ^/^ doc
        ) empty F


  (* Basis *)

  fun ppBasis (T, F, G, E) =
      vbox(
        PPStaticEnv.ppTyNameSet T ^/^
        ppSigEnv G ^/^
        ppFunEnv F ^/^
        PPStaticEnv.ppEnv E ^/^
        text ""
      )
end;
