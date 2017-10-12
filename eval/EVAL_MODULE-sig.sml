(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML modules evaluation
 *
 * Definition, Section 7.3
 * + RFC: Local modules
 *
 * Notes:
 *   - State is passed as reference and modified via side effects. This way
 *     expanding out the state and exception convention in the inference rules
 *     can be avoided (would really be a pain). Note that the state therefore
 *     never is returned.
 *   - Doing so, we can model the exception convention using exceptions.
 *)

signature EVAL_MODULE =
sig
    (* Import types *)

    type StrDec = GrammarModule.StrDec
    type TopDec = GrammarModule.TopDec
    type Env    = DynamicObjectsCore.Env
    type Basis  = DynamicObjectsModule.Basis
    type State  = EvalCore.State


    (* Export *)

    val evalStrDec : State ref * Env * StrDec -> Env
    val evalTopDec : State ref * Basis * TopDec -> Basis
end;
