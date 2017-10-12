(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML programs
 *
 * Definition, Section 8
 *
 * Note: see EVAL_CORE-sig.sml
 *)

structure Program : PROGRAM =
struct
  (* Import *)

  open SyntaxProgram
  open AnnotationProgram
  open Basis

  type State = EvalModule.State


  (* Helpers for output *)

  val width = 79

  fun printPack(s, (e, loc)) =
        Error.print(loc,
          "uncaught exception " ^
          PrettyPrint.toString(PPVal.ppExVal(s, e), width)
        )

  fun printStaticBasis B_STAT =
      ( PrettyPrint.output(TextIO.stdOut, PPStaticBasis.ppBasis B_STAT, width);
        TextIO.flushOut TextIO.stdOut
      )

  fun printDynamicBasis(s, B_DYN) =
      ( PrettyPrint.output(
          TextIO.stdOut, PPDynamicBasis.ppBasis(s, B_DYN), width);
        TextIO.flushOut TextIO.stdOut
      )

  fun printBasis(s, B) =
      ( PrettyPrint.output(TextIO.stdOut, PPBasis.ppBasis(s, B), width);
        TextIO.flushOut TextIO.stdOut
      )


  (* Helpers for basis modification *)

  val oplus = Basis.oplus

  infix oplus
  infix -->

  fun ?exec echo (B, NONE)        default = default
    | ?exec echo (B, SOME phrase) default = exec echo (B, phrase)


  (* Inference rules [Section 8] *)

  fun execProgram echo ((s, B), Program(topdec, program_opt)@@A) =
      (* [Rule 189] *)
      let
        val B_STAT1 = ElabModule.elabTopDec(Basis.B_STATof B, topdec)
        val B_DYN1  = EvalModule.evalTopDec((s, Basis.B_DYNof B), topdec)
        val _       = if echo then printBasis(!s, (B_STAT1, B_DYN1)) else ()
        val B'      = B oplus (B_STAT1, B_DYN1)
        val B''     = ?execProgram echo ((s, B'), program_opt) B'
      in
        B''
      end
        handle
          Error.Error =>
            (* [Rule 187] *)
            let
              val B' = ?execProgram echo ((s, B), program_opt) B
            in
              B'
            end
        | DynamicObjectsCore.Pack p =>
            (* [Rule 188] *)
            let
              val _  = printPack(!s, p)
              val B' = ?execProgram echo ((s, B), program_opt) B
            in
              B'
            end


  (* Elaboration mode *)

  fun elabProgram echo (B_STAT, Program(topdec, program_opt)@@A) =
      let
        val B_STAT1  = ElabModule.elabTopDec(B_STAT, topdec)
        val  _       = if echo then printStaticBasis B_STAT1 else ()
        val B_STAT'  = StaticBasis.plus(B_STAT, B_STAT1)
        val B_STAT'' = ?elabProgram echo (B_STAT', program_opt) B_STAT'
      in
        B_STAT''
      end
        handle Error.Error =>
          B_STAT


  (* Evaluation mode *)

  fun evalProgram echo ((s, B_DYN), Program(topdec, program_opt)@@A) =
      let
        val B_DYN1  = EvalModule.evalTopDec((s, B_DYN), topdec)
        val  _      = if echo then printDynamicBasis(!s, B_DYN1) else ()
        val B_DYN'  = DynamicBasis.plus(B_DYN, B_DYN1)
        val B_DYN'' = ?evalProgram echo ((s, B_DYN'), program_opt) B_DYN'
      in
        B_DYN''
      end
        handle
          Error.Error =>
            (* Runtime error *)
            let
              val B_DYN' = ?evalProgram echo ((s, B_DYN), program_opt) B_DYN
            in
              B_DYN'
            end
        | DynamicObjectsCore.Pack p =>
            let
              val _      = printPack(!s, p)
              val B_DYN' = ?evalProgram echo ((s, B_DYN), program_opt) B_DYN
            in
              B_DYN'
            end
end;
