(* OS.Path.mk{Absolute,Relative} changed signature *)

structure OS =
struct
  open OS

  structure Path =
  struct
    open Path

    fun mkAbsolute{path, relativeTo} = Path.mkAbsolute(path, relativeTo)
    fun mkRelative{path, relativeTo} = Path.mkRelative(path, relativeTo)
  end
end
