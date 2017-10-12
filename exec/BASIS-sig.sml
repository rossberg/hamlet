(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML combined basis
 *
 * Definition, Section 8
 *)

signature BASIS =
sig
  (* Import *)

  type StaticBasis  = StaticObjectsModule.Basis                 (* [B_STAT] *)
  type DynamicBasis = DynamicObjectsModule.Basis                (* [B_DYN] *)


  (* Type [Section 8] *)

  type Basis = StaticBasis * DynamicBasis                       (* [B] *)


  (* Operations *)

  val B_STATof : Basis -> StaticBasis
  val B_DYNof  : Basis -> DynamicBasis

  val oplus    : Basis * Basis -> Basis
end;
