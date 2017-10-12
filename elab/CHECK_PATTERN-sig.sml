(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML consistency of patterns and matches
 *
 * Definition, Section 4.11
 *
 * Note:
 *   The requirement to check for irredundancy of matches is a `bug' in the
 *   Definition since it cannot be checked in general, for two reasons:
 *
 *   (1) There may be (hidden) aliasing of exception constructors.
 *       Consequently, we only detect redundant exception constructors
 *       if they are denoted by the same longvid.
 *
 *   (2) There is no requirement of consistency for constructors in
 *       sharing specifications or type realisations (actually, we
 *       consider that a serious bug). For example,
 *         datatype t1 = A | B
 *         datatype t2 = C
 *         sharing type t1 = t2
 *       is a legal specification. This allows a mix of the constructors
 *       to appear in matches, rendering the terms of irredundancy and
 *       exhaustiveness meaningless. We make no attempt to detect this,
 *       so generated warnings may or may not make sense in that situation.
 *)

signature CHECK_PATTERN =
sig
  (* Import *)

  type Pat        = SyntaxCore.Pat
  type Match      = SyntaxCore.Match

  type Env        = StaticEnv.Env
  type Exhaustive = StaticObjectsCore.Exhaustive


  (* Operations *)

  val checkPat   : Env * Pat   -> Exhaustive
  val checkMatch : Env * Match -> Exhaustive
end;
