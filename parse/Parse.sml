(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML syntactic analysis
 *)

structure Parse : PARSE =
struct
    (* Import *)

    type source  = Source.source
    type InfEnv  = Infix.InfEnv
    type Program = GrammarProgram.Program


    (* Build Yacc parser *)

    structure LrVals = LrValsFn(structure Token      = LrParser.Token)
    structure Lexer  = LexerFn (structure Tokens     = LrVals.Tokens)
    structure Lexer' = LineAwareLexer(structure Lexer = Lexer
				      exception Error = Source.Error)
    structure Parser = Join    (structure LrParser   = LrParser
				structure ParserData = LrVals.ParserData
				structure Lex        = Lexer')


    (* The actual parsing function *)

    fun parse(J, source, filename) =
	let
	    val yyread = ref false
	    fun yyinput _ =
		if !yyread then
		    ""
		else
		    ( yyread := true; source )

	    val lexer = Parser.makeLexer yyinput

	    fun onError(s, pos1, pos2) =
		Error.error({file = filename, region = (pos1,pos2)}, s)

	    fun I(left, right) : Source.info =
		{ file = filename,
		  region = (left, if right = (0,0) then left else right) }

	    val ((program,J'),_) =
		Parser.parse(0, lexer, onError, (I, J))
		handle Lexer'.Error(region, e) =>
		    Error.error({file = filename, region = region}, e)
	in
	    (J',program)
	end
end;
