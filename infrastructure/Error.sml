(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Error handling.
 *)

structure Error : ERROR =
struct
    (* Import *)

    type position = Source.info


    (* Export *)

    exception Error

    val dir = OS.FileSys.getDir()

    fun print({file, region = ((line1,col1), (line2,col2))}, message) =
	( case file
	    of NONE      => ()
	     | SOME name => 
	       let
		   val name' = OS.Path.mkRelative{path=name, relativeTo=dir}
	       in
		   TextIO.output(TextIO.stdErr, name' ^ ":")
	       end
	; TextIO.output(TextIO.stdErr, Int.toString line1 ^ ".")
	; TextIO.output(TextIO.stdErr, Int.toString col1 ^ "-")
	; TextIO.output(TextIO.stdErr, Int.toString line2 ^ ".")
	; TextIO.output(TextIO.stdErr, Int.toString col2 ^ ": ")
	; TextIO.output(TextIO.stdErr, message ^ "\n")
	; TextIO.flushOut TextIO.stdErr
	)

    fun warning(pos, message) =
	print(pos, "warning: " ^ message)

    fun error(pos, message) =
	( print(pos, message)
	; raise Error
	)

    fun errorLab  (pos, s, lab)   = error(pos, s ^ Lab.toString lab)
    fun errorVId  (pos, s, vid)   = error(pos, s ^ VId.toString vid)
    fun errorTyCon(pos, s, tycon) = error(pos, s ^ TyCon.toString tycon)
    fun errorTyVar(pos, s, tyvar) = error(pos, s ^ TyVar.toString tyvar)
    fun errorStrId(pos, s, strid) = error(pos, s ^ StrId.toString strid)
    fun errorSigId(pos, s, sigid) = error(pos, s ^ SigId.toString sigid)

    fun errorLongVId(pos, s, longvid) = error(pos, s ^ LongVId.toString longvid)
    fun errorLongTyCon(pos, s, longtycon) =
	    error(pos, s ^ LongTyCon.toString longtycon)
    fun errorLongStrId(pos, s, longstrid) =
	    error(pos, s ^ LongStrId.toString longstrid)
    fun errorLongSigId(pos, s, longsigid) =
	    error(pos, s ^ LongSigId.toString longsigid)
end;
