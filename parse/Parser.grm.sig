signature Parser_TOKENS =
sig
type ('a,'b) token
type svalue
val LONGID: (string list*string) *  'a * 'a -> (svalue,'a) token
val ETYVAR: (string) *  'a * 'a -> (svalue,'a) token
val TYVAR: (string) *  'a * 'a -> (svalue,'a) token
val STAR:  'a * 'a -> (svalue,'a) token
val SYMBOL: (string) *  'a * 'a -> (svalue,'a) token
val ALPHA: (string) *  'a * 'a -> (svalue,'a) token
val CHAR: (string) *  'a * 'a -> (svalue,'a) token
val STRING: (string) *  'a * 'a -> (svalue,'a) token
val REAL: (string) *  'a * 'a -> (svalue,'a) token
val HEXWORD: (string) *  'a * 'a -> (svalue,'a) token
val WORD: (string) *  'a * 'a -> (svalue,'a) token
val HEXINT: (string) *  'a * 'a -> (svalue,'a) token
val INT: (string) *  'a * 'a -> (svalue,'a) token
val NUMERIC: (string) *  'a * 'a -> (svalue,'a) token
val DIGIT: (string) *  'a * 'a -> (svalue,'a) token
val ZERO:  'a * 'a -> (svalue,'a) token
val SEAL:  'a * 'a -> (svalue,'a) token
val WHERE:  'a * 'a -> (svalue,'a) token
val STRUCTURE:  'a * 'a -> (svalue,'a) token
val STRUCT:  'a * 'a -> (svalue,'a) token
val SIGNATURE:  'a * 'a -> (svalue,'a) token
val SIG:  'a * 'a -> (svalue,'a) token
val SHARING:  'a * 'a -> (svalue,'a) token
val INCLUDE:  'a * 'a -> (svalue,'a) token
val FUNCTOR:  'a * 'a -> (svalue,'a) token
val EQTYPE:  'a * 'a -> (svalue,'a) token
val HASH:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val DARROW:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val UNDERBAR:  'a * 'a -> (svalue,'a) token
val DOTS:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val WITHTYPE:  'a * 'a -> (svalue,'a) token
val WITH:  'a * 'a -> (svalue,'a) token
val VAL:  'a * 'a -> (svalue,'a) token
val TYPE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val REC:  'a * 'a -> (svalue,'a) token
val RAISE:  'a * 'a -> (svalue,'a) token
val ORELSE:  'a * 'a -> (svalue,'a) token
val OPEN:  'a * 'a -> (svalue,'a) token
val OP:  'a * 'a -> (svalue,'a) token
val OF:  'a * 'a -> (svalue,'a) token
val NONFIX:  'a * 'a -> (svalue,'a) token
val LOCAL:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val INFIXR:  'a * 'a -> (svalue,'a) token
val INFIX:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val HANDLE:  'a * 'a -> (svalue,'a) token
val FUN:  'a * 'a -> (svalue,'a) token
val FN:  'a * 'a -> (svalue,'a) token
val EXCEPTION:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val DATATYPE:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val AS:  'a * 'a -> (svalue,'a) token
val ANDALSO:  'a * 'a -> (svalue,'a) token
val AND:  'a * 'a -> (svalue,'a) token
val ABSTYPE:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Parser_LRVALS=
sig
structure Tokens : Parser_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
