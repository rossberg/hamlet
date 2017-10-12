(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML syntactic restrictions for programs
 *)

structure SyntacticRestrictionsProgram : SYNTACTIC_RESTRICTIONS_PROGRAM =
struct
  (* Import *)

  open SyntaxProgram
  open AnnotationProgram

  type Basis = SyntacticRestrictionsModule.Basis


  (* Helpers *)

  val plus = BindingBasis.plus

  infix plus

  fun ?check(B, NONE)        default = default
    | ?check(B, SOME phrase) default = check(B, phrase)


  (* Operation *)

  fun checkProgram(B, Program(topdec, program_opt)@@A) =
      let
        val B1  = SyntacticRestrictionsModule.checkTopDec(B, topdec)
        val B'  = B plus B1
      in
        ?checkProgram(B', program_opt) B'
      end
end;
