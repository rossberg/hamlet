(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note: Incomplete.
 *)

signature OS =
sig
  structure FileSys : OS_FILE_SYS
  structure Path : OS_PATH
  structure Process : OS_PROCESS
(*
  structure IO : OS_IO
*)
  eqtype syserror
  exception SysErr of string * syserror option
(*
  val errorMsg : syserror -> string
  val errorName : syserror -> string
  val syserror : string -> syserror option
*)
end;
