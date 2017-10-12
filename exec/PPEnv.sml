(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of the combined static/dynamic environment
 *)

structure PPEnv : PP_ENV =
struct
  (* Import *)

  type Env   = StaticObjectsCore.Env * DynamicObjectsCore.Env
  type State = DynamicObjectsCore.State

  open PrettyPrint
  open PPMisc

  infixr ^^ ^/^


  (* Simple objects *)

  fun ppVId vid     = text(VId.toString vid)
  fun ppStrId strid = text(StrId.toString strid)


  (* Environments *)

  fun ppValEnv(s, (VE_STAT, VE_DYN)) =
      VIdMap.foldri
        (fn(vid, (sigma, IdStatus.v), doc) =>
          let
            val (v, is) = valOf(VIdMap.find(VE_DYN, vid))
          in
            fbox(nest(
              hbox(text "val" ^/^ ppVId vid ^/^ text "=") ^/^
                PPVal.ppVal(s, v) ^/^
                text ":" ^/^
                PPType.ppTypeScheme sigma
            )) ^/^ doc
          end
        | (vid, (sigma, _), doc) => doc
        ) empty VE_STAT

  fun ppStrEnv(s, T, (SE_STAT, SE_DYN)) =
      StrIdMap.foldri
        (fn(strid, E_STAT, doc) =>
          let
            val E_DYN = valOf(StrIdMap.find(SE_DYN, strid))
          in
            fbox(nest(
              hbox(text "structure" ^/^ ppStrId strid ^/^ text "=") ^/^
                ppStr (s, T, (E_STAT, E_DYN))
            )) ^/^ doc
          end
        ) empty SE_STAT

  and ppEnv'(s, T,
        ( StaticObjectsCore.Env(SE_STAT, TE_STAT, VE_STAT),
          DynamicObjectsCore.Env(SE_DYN, TE_DYN, VE_DYN) )
      ) =
      vbox(
        ppStrEnv(s, T, (SE_STAT, SE_DYN)) ^/^
        PPStaticEnv.ppTyEnv(T, TE_STAT) ^/^
        PPStaticEnv.ppExEnv VE_STAT ^/^
        ppValEnv(s, (VE_STAT, VE_DYN))
      )

  and ppEnv(s, (E_STAT, E_DYN)) =
        ppEnv'(s, TyNameSet.empty, (E_STAT, E_DYN))


  (* Structures *)

  and ppStr(s, T, E) =
      abox(below(
        nest(
          text "struct" ^/^
          vbox(
            ppEnv'(s, T, E)
          )
        ) ^/^ text "end"
      ))
end;
