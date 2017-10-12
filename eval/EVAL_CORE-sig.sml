(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML core evaluation
 *
 * Definition, Section 6.7
 * + RFC: Local modules
 * + RFC: First-class modules
 *
 * Notes:
 *   - State is passed as reference and modified via side effects. This way
 *     expanding out of the state and exception convention in the inference
 *     rules can be avoided (would really be a pain). Note that the state
 *     therefore never is returned.
 *   - Doing so, we can model the exception convention using exceptions.
 *)

signature EVAL_CORE =
sig
    (* Import types *)

    type AtExp = GrammarCore.AtExp
    type Dec   = GrammarCore.Dec
    type Val   = DynamicObjectsCore.Val
    type Env   = DynamicObjectsCore.Env
    type Mod   = DynamicObjectsCore.Mod
    type State = DynamicObjectsCore.State

    (* Recursive import *)

    structure EvalModule :
    sig
	val evalStrExp : (State ref * Env * GrammarModule.StrExp -> Mod) ref
	val evalStrDec : (State ref * Env * GrammarCore.StrDec' -> Env) ref
    end

    (* Export *)

    val evalAtExp : State ref * Env * AtExp -> Val
    val evalDec :   State ref * Env * Dec -> Env
end;
