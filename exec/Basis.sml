(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML combined basis
 *
 * Definition, Section 8
 *)

structure Basis :> BASIS =
struct
  (* Import *)

  type StaticBasis  = StaticObjectsModule.Basis                 (* [B_STAT] *)
  type DynamicBasis = DynamicObjectsModule.Basis                (* [B_DYN] *)


  (* Type [Section 8] *)

  type Basis = StaticBasis * DynamicBasis                       (* [B] *)


  (* Projections *)

  fun B_STATof(B_STAT, B_DYN) = B_STAT
  fun B_DYNof(B_STAT, B_DYN)  = B_DYN


  (* Modification [Section 4.3] *)

  infix oplus

  fun (B_STAT, B_DYN) oplus (B_STAT', B_DYN') =
      (StaticBasis.plus(B_STAT, B_STAT'), DynamicBasis.plus(B_DYN, B_DYN'))
end;
