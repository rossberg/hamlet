(*
 * (c) Andreas Rossberg 2013
 *
 * Standard ML program elaboration
 *
 * Definition, Section 8
 *
 * Note: see ELAB_PROGRAM-sig.sml
 *)

structure ElabProgram : ELAB_PROGRAM =
struct
  (* Import types *)

  open SyntaxProgram
  open AnnotationProgram
  open StaticObjectsModule
  open Error


  (* Helpers for basis modification and side conditions *)

  val plus = StaticBasis.plus

  infix plus
  infix -->

  fun ?elab(B, NONE)        default = default
    | ?elab(B, SOME phrase) default = elab(B, phrase)


  (* Programs *)

  fun elabProgram(B, Program(topdec, program_opt)@@A) =
      (* [cf. Rule 188 or 189] *)
      let
        val B1  = ElabModule.elabTopDec(B, topdec)
        val B'  = B plus B1
        val B'' = ?elabProgram(B', program_opt) B'
      in
        B''
      end --> elab A
end;
