(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML pretty printing of the dynamic environment
 *)

structure PPDynamicEnv : PP_DYNAMIC_ENV =
struct
  (* Import *)

  type Env   = DynamicObjectsCore.Env
  type State = DynamicObjectsCore.State

  open PrettyPrint
  open PPMisc

  infixr ^^ ^/^


  (* Simple objects *)

  fun ppVId vid     = text(VId.toString vid)
  fun ppTyCon tycon = text(TyCon.toString tycon)
  fun ppStrId strid = text(StrId.toString strid)


  (* Environments *)

  fun ppValEnv(s, VE) =
      VIdMap.foldri
        (fn(vid, (v, IdStatus.v), doc) =>
          fbox(nest(
            hbox(text "val" ^/^ ppVId vid ^/^ text "=") ^/^
              PPVal.ppVal(s, v)
          )) ^/^ doc
        | (vid, (v, _), doc) => doc
        ) empty VE

  fun ppExEnv VE =
      VIdMap.foldri
        (fn(vid, (v, IdStatus.e), doc) =>
          hbox(text "exception" ^/^ ppVId vid) ^/^ doc
        | (vid, (v, _), doc) => doc
        ) empty VE

  fun ppConEnv VE =
      VIdMap.foldli
        (fn(vid, (v, IdStatus.c), doc) =>
          hbox(text "con" ^/^ ppVId vid) ^/^ doc
        | (vid, (v, _), doc) => doc
        ) empty VE

  fun ppTyEnv(s, TE) =
      TyConMap.foldri
        (fn(tycon, VE, doc) =>
          fbox(nest(
            hbox(text "type" ^/^ ppTyCon tycon)
          )) ^/^ doc
        ) empty TE

  fun ppStrEnv(s, SE) =
      StrIdMap.foldri
        (fn(strid, S, doc) =>
          fbox(nest(
            hbox(text "structure" ^/^ ppStrId strid ^/^ text "=") ^/^
              ppStr(s, S)
          )) ^/^ doc
        ) empty SE

  and ppEnv(s, DynamicObjectsCore.Env(SE, TE, VE)) =
      vbox(
        ppStrEnv(s, SE) ^/^
        ppTyEnv(s, TE) ^/^
        ppConEnv VE ^/^
        ppExEnv  VE ^/^
        ppValEnv(s, VE)
      )


  (* Structures *)

  and ppStr(s, E) =
      abox(below(
        nest(
          text "struct" ^/^
          vbox(
            ppEnv(s, E)
          )
        ) ^/^ text "end"
      ))
end;
