(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of programs into JavaScript.
 *)

structure CompileJS : COMPILE_JS =
struct
  (* Import *)

  type Program     = SyntaxProgram.Program
  type StaticBasis = StaticObjectsModule.Basis


  (* Helpers for output *)

  val width = 79

  fun printStmts out stmts =
      let
        val docs = List.map PPJS.ppStmt stmts
        val doc  = List.foldr PrettyPrint.^/^ (PrettyPrint.text "") docs
      in
        PrettyPrint.output(out, PrettyPrint.vbox doc, width);
        TextIO.flushOut out
      end


  (* Export *)

  fun compileProgram out (B, program) =
      let
        val B'    = ElabProgram.elabProgram(B, program)
        val stmts = JSTranslateProgram.translateProgram(B, program)
      in
        printStmts out stmts;
        B'
      end
end;
