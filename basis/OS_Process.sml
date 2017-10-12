(*
 * (c) Andreas Rossberg 2013
 *
 * Standard ML Basis Library
 *)

structure OS =
struct
  open OS

  structure Process :> OS_PROCESS =
  struct
    type status = int

    val success = 0
    val failure = 1

    fun isSuccess st = st = 0
    fun terminate st = (use{b = "OS.Process.terminate"} : status -> 'a) st
    val exit = terminate
  end
end;
