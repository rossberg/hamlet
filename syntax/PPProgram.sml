(*
 * (c) Andreas Rossberg 2007-2013
 *
 * Printer for abstract program syntax
 *)

structure PPProgram : PP_PROGRAM =
struct
  (* Import *)

  open SyntaxProgram
  open Annotation
  open PPSyntax

  (* Programs *)

  fun ppProgram(out, i, Program(topdec, program_opt)@@A) =
        ppElem(out, i, "Program", A,
          [sub PPModule.ppTopDec topdec, subo ppProgram program_opt])
end;
