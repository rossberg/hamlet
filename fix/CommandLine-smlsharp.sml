(* Under SML# we don't want to know the location of the runtime,
 * but need to know the image path so that we find the basis lib.
 * For now, we fake it with the CWD. *)

structure CommandLine : COMMAND_LINE =
struct
  val arguments = CommandLine.arguments

  fun name() = OS.FileSys.getDir() ^ "/hamlet"
end
