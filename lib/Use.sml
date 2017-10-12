(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library `use' function
 *)

structure Use : USE =
struct
  val inn = ref [] : string list ref
  val out = ref [] : string list ref

  fun enqueue name =
      if OS.Path.isAbsolute name then
        inn := name :: !inn
      else
        let
          val dir  = OS.FileSys.getDir()
          val path = OS.Path.mkAbsolute{path = name, relativeTo = dir}
        in
          inn := path :: !inn
        end

  fun extract() =
      case (!inn, !out) of
        ([], [])   => NONE
      | (_, s::ss) => ( out := ss; SOME s )
      | (ss, [])   => ( inn := []; out := List.rev ss; extract() )
end;
