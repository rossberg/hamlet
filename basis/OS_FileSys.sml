(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note: Incomplete.
 *)

structure OS =
struct
  open OS

  structure FileSys : OS_FILE_SYS =
  struct
    val getDir = use{b = "OS.FileSys.getDir"} : unit -> string
    val chDir  = use{b = "OS.FileSys.chDir"} : string -> unit
    val mkDir  = use{b = "OS.FileSys.mkDir"} : string -> unit
    val rmDir  = use{b = "OS.FileSys.rmDir"} : string -> unit
    val isDir  = use{b = "OS.FileSys.isDir"} : string -> bool
  end
end;
