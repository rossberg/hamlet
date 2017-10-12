(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML consistency of patterns and matches
 *
 * Definition, Section 4.11
 * + RFC: Views
 *
 * Note:
 *     The requirement to check for irredundancy of matches is a `bug' in the
 *     definition since this cannot be checked in general for two reasons:
 *
 *     (1) There may be (hidden) aliasing of exception constructors.
 *         Consequently, we only detect redundant exception constructors
 *         if they are denoted by the same longvid.
 *
 *     (2) There is no requirement of consistency for constructors in
 *         sharing specifications or type realisations (actually, we
 *         consider this a serious bug). For example,
 *		datatype t1 = A | B
 *		datatype t2 = C
 *		sharing type t1 = t2
 *         is a legal specification. This allows a mix of the constructors
 *         to appear in matches, rendering the terms of irredundancy and
 *         exhaustiveness meaningless. We make no attempt to detect this,
 *         so generated warnings may or may not make sense in that situation.
 *)

signature CHECK_PATTERN =
sig
    (* Import *)

    type Pat   = GrammarCore.Pat
    type Match = GrammarCore.Match
    type Env   = StaticEnv.Env


    (* Operations *)

    val isExhaustive :	Env * Pat   -> bool
    val checkPat :	Env * Pat   -> unit
    val checkMatch :	Env * Match -> unit
    val viewPat :	Env * Pat   -> unit
    val viewMatch :	Env * Match -> unit
end;
