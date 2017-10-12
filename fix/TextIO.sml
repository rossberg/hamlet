(* The type of inputLine changed. *)

structure TextIO =
struct
  open TextIO

  fun inputLine strm =
      case inputLine strm of
        "" => NONE
      | s  => SOME s
end
