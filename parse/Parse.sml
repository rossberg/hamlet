(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML syntactic analysis
 *)

structure Parse : PARSE =
struct
  (* Import *)

  type source  = Source.source
  type InfEnv  = Infix.InfEnv
  type Program = SyntaxProgram.Program


  (* Build Yacc parser *)

  exception RawError of (int * int) * string
  exception LocError of Source.region * string

  structure LrVals = LrValsFn(structure Token = LrParser.Token)
  structure RawLexer =
      LexerFn(
        structure Tokens = LrVals.Tokens
        exception RawError = RawError
      )
  structure LocLexer =
      LocLexer(
        structure RawLexer = RawLexer
        exception RawError = RawError
        exception LocError = LocError
      )
  structure Parser =
      Join(
        structure LrParser = LrParser
        structure ParserData = LrVals.ParserData
        structure Lex = LocLexer
      )


  (* The actual parsing function *)

  fun parse(J, source, file) =
      let
        val yyread = ref false
        fun yyinput _ = if !yyread then "" else ( yyread := true; source )

        val lexer = Parser.makeLexer yyinput

        fun onError(s, pos1, pos2) =
            Error.error({file = file, region = (pos1, pos2)}, s)

        fun A(l, r) = {file = file, region = (l, if r = (0, 0) then l else r)}

        val ((program, J'), _) =
            Parser.parse(0, lexer, onError, (A, J))
            handle LocError(region, e) =>
              Error.error({file = file, region = region}, e)
      in
        (J', program)
      end
end;
