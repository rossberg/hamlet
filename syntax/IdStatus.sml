(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML identifier status
 *
 * Definition, Sections 4.1 and 5.5
 *)

structure IdStatus :> IDSTATUS =
struct
  (* Type [Section 4.1] *)

  datatype IdStatus = c | e | v                 (* [is] *)

  (* Generalisation [Section 5.5] *)

  fun generalises(is1, is2) = is1 = is2 orelse is2 = v
end;
