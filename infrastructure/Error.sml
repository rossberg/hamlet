(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Error handling.
 *)

structure Error : ERROR =
struct
  (* Import *)

  type loc = Source.loc


  (* Export *)

  exception Error

  val dir = OS.FileSys.getDir()

  fun print({file, region = ((line1, col1), (line2, col2))}, message) =
      ( case file of
          NONE      => ()
        | SOME name => 
            TextIO.output(TextIO.stdErr, 
              OS.Path.mkRelative{path = name, relativeTo = dir} ^ ":");
        TextIO.output(TextIO.stdErr, Int.toString line1 ^ ".");
        TextIO.output(TextIO.stdErr, Int.toString col1 ^ "-");
        TextIO.output(TextIO.stdErr, Int.toString line2 ^ ".");
        TextIO.output(TextIO.stdErr, Int.toString col2 ^ ": ");
        TextIO.output(TextIO.stdErr, message ^ "\n");
        TextIO.flushOut TextIO.stdErr
      )

  fun warning(loc, message)     = print(loc, "warning: " ^ message)
  fun error(loc, message)       = ( print(loc, message); raise Error )

  fun errorLab(loc, s, lab)     = error(loc, s ^ Lab.toString lab)
  fun errorVId(loc, s, vid)     = error(loc, s ^ VId.toString vid)
  fun errorTyCon(loc, s, tycon) = error(loc, s ^ TyCon.toString tycon)
  fun errorTyVar(loc, s, tyvar) = error(loc, s ^ TyVar.toString tyvar)
  fun errorStrId(loc, s, strid) = error(loc, s ^ StrId.toString strid)
  fun errorSigId(loc, s, sigid) = error(loc, s ^ SigId.toString sigid)
  fun errorFunId(loc, s, funid) = error(loc, s ^ FunId.toString funid)

  fun errorLongVId(loc, s, longvid) =
        error(loc, s ^ LongVId.toString longvid)
  fun errorLongTyCon(loc, s, longtycon) =
        error(loc, s ^ LongTyCon.toString longtycon)
  fun errorLongStrId(loc, s, longstrid) =
        error(loc, s ^ LongStrId.toString longstrid)
end;
