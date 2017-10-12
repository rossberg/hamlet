(*
 * (c) Andreas Rossberg 2013
 *
 * Standard ML program evaluation
 *
 * Definition, Section 8
 *
 * Notes: see EVAL_PROGRAM-sig.sml
 *)

structure EvalProgram : EVAL_PROGRAM =
struct
  (* Import *)

  open SyntaxProgram
  open AnnotationProgram
  open DynamicObjectsModule
  open Error

  type State = EvalCore.State


  (* Helpers for basis modification *)

  val plus = DynamicBasis.plus

  infix plus

  fun ?eval(B, NONE)        default = default
    | ?eval(B, SOME phrase) default = eval(B, phrase)


  (* Programs *)

  fun evalProgram((s, B), Program(topdec, program_opt)@@A) =
      let
        val B1  = EvalModule.evalTopDec((s, B), topdec)
        val B'  = B plus B1
        val B'' = ?evalProgram((s, B'), program_opt) B'
      in
        B''
      end
end;
