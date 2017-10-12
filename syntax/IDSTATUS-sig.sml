(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML identifier status
 *
 * Definition, Sections 4.1 and 5.5
 *)

signature IDSTATUS =
sig
  (* Type [Section 4.1] *)

  datatype IdStatus = c | e | v                 (* [is] *)

  (* Operations *)

  val generalises : IdStatus * IdStatus -> bool
end;
