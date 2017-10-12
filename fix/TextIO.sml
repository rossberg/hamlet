(* The type of inputLine changed. *)

structure TextIO =
struct
    open TextIO

    val inputLine = fn strm =>
	case inputLine strm
	 of "" => NONE
	  | s  => SOME s
end
