(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML lexical analysis
 *
 * Definition, Sections 2.1-2.5, 3.1
 *
 * Notes:
 *   Since all lexical classes must be disjoint:
 *   - There is no single class ID, use ALPHA|SYMBOL|STAR|EQUALS.
 *   - There is no class LAB, use ALPHA|SYMBOL|NUMERIC|DIGIT|STAR.
 *   - ID does not contain `=' and `*', use ID|EQUALS|STAR.
 *   - LONGID does not contain unqualified ids (but allows for `=' and `*').
 *   - INT does not contain positive decimal integers without leading 0,
 *     and single DIGIT integers, use INT|NUMERIC|DIGIT|ZERO.
 *   - NUMERIC does not contain single digit numbers, use NUMERIC|DIGIT.
 *   - DIGIT does not contain 0, use DIGIT|ZERO.
 *
 *   The lexer uses a global variable to recognise nested comments, so it is
 *   not reentrant.
 *)

  (* Types to match structure LEXER.UserDeclaration *)

  type ('a,'b) token = ('a,'b) Tokens.token
  type pos           = int
  type svalue        = Tokens.svalue
  type lexresult     = (svalue, pos) token


  (* Handling nested comments *)

  val nesting = ref 0

  fun eof() =
      if !nesting = 0 then
    	Tokens.EOF(~1, ~1)
      else
	raise RawError((0, 0), "unclosed comment")


  (* Some helpers to create tokens *)

  open Tokens

  fun toLRPos(yypos, yytext) =
      let
	val yypos = yypos - 2	(* bug in ML-Lex... *)
      in
	(yypos, yypos + String.size yytext)
      end

  fun token(yypos, yytext, TOKEN) = TOKEN(toLRPos(yypos, yytext))

  fun tokenOf(yypos, yytext, TOKEN, toVal) =
      let
	val (l, r) = toLRPos(yypos, yytext)
      in
	TOKEN(toVal yytext, l, r)
      end

  fun error(yypos, yytext, s) = raise RawError(toLRPos(yypos, yytext), s)

  fun invalid(yypos, yytext) =
      let
	val s = "invalid character `" ^ String.toString yytext ^ "'"
      in
	error(yypos, yytext, s)
      end


  (* Convert identifiers *)

  fun toId s = s

  fun toLongId s =
      let
	fun split[] = raise Fail "Lexer.toLongId: empty longid"
	  | split[x] = ([], x)
	  | split(x::xs) = let val (ys, y) = split xs in (x::ys, y) end
      in
	split(String.fields (fn c => c = #".") s)
      end


  (* Convert constants *)

  fun toInt s     = s
  fun toHexInt s  = if String.sub(s, 0) = #"~" then
                      "~" ^ String.extract(s, 3, NONE)
                    else
                      String.extract(s, 2, NONE)
  fun toWord s    = String.extract(s, 2, NONE)
  fun toHexWord s = String.extract(s, 3, NONE)
  fun toReal s    = s
  fun toString s  = String.extract(s, 1, SOME(String.size s - 2))
  fun toChar s    = String.extract(s, 2, SOME(String.size s - 3))

%%

%header	(
  functor LexerFn(
    structure Tokens : Parser_TOKENS
    exception RawError of (int * int) * string
  )
);

%s COMMENT;

%full

  formatting = [\ \t\n\011\012\013]+;
  letter     = [A-Za-z];
  symbol     = [-!%&$#+/:<=>?@\\~`|*^];
  digit      = [0-9];
  hexdigit   = [0-9a-fA-F];

  posdecint  = {digit}+;
  poshexint  = "0x"{hexdigit}+;
  negdecint  = "~"{posdecint};
  neghexint  = "~"{poshexint};
  decint     = {posdecint} | {negdecint};
  hexint     = {poshexint} | {neghexint};
  decword    = "0w"{digit}+;
  hexword    = "0wx"{hexdigit}+;

  exp        = "E" | "e";
  real       = ({decint}"."{digit}+ ({exp}{decint})?) | ({decint}{exp}{decint});

  numericlab = [1-9]{digit}*;
  alphanumid = {letter}({letter} | {digit} | [_'])*;
  symbolicid = {symbol}+;
  id         = {alphanumid} | {symbolicid};
  tyvar      = "'"({letter} | {digit} | [_'])*;
  longid     = ({alphanumid}".")+ ({id}|"="|"*");

  printable  = [^\000-\032"\127\\];
  escape     = "\\a" | "\\b" | "\\t" | "\\n" | "\\v" | "\\f" | "\\r" |
	       ("\\^"[@-_])  | ("\\"{digit}{3})  | ("\\u"{hexdigit}{4}) |
	       "\\\"" | "\\\\";
  gap        = ("\\"{formatting}"\\");
  stringchar = {printable} | " " | {escape};
  string     = "\""({stringchar} | {gap})*"\"";
  char       = "#\""{gap}*{stringchar}{gap}*"\"";

%%

  <INITIAL>{formatting}	=> ( continue() );


  <INITIAL>"#"		=> ( token(yypos, yytext, HASH) );
  <INITIAL>"("		=> ( token(yypos, yytext, LPAR) );
  <INITIAL>")"		=> ( token(yypos, yytext, RPAR) );
  <INITIAL>"*"		=> ( token(yypos, yytext, STAR) );
  <INITIAL>","		=> ( token(yypos, yytext, COMMA) );
  <INITIAL>"->"		=> ( token(yypos, yytext, ARROW) );
  <INITIAL>"..."	=> ( token(yypos, yytext, DOTS) );
  <INITIAL>":"		=> ( token(yypos, yytext, COLON) );
  <INITIAL>":>"		=> ( token(yypos, yytext, SEAL) );
  <INITIAL>";"		=> ( token(yypos, yytext, SEMICOLON) );
  <INITIAL>"="		=> ( token(yypos, yytext, EQUALS) );
  <INITIAL>"=>"		=> ( token(yypos, yytext, DARROW) );
  <INITIAL>"["		=> ( token(yypos, yytext, LBRACK) );
  <INITIAL>"]"		=> ( token(yypos, yytext, RBRACK) );
  <INITIAL>"_"		=> ( token(yypos, yytext, UNDERBAR) );
  <INITIAL>"{"		=> ( token(yypos, yytext, LBRACE) );
  <INITIAL>"|"		=> ( token(yypos, yytext, BAR) );
  <INITIAL>"}"		=> ( token(yypos, yytext, RBRACE) );

  <INITIAL>"abstype"	=> ( token(yypos, yytext, ABSTYPE) );
  <INITIAL>"and"	=> ( token(yypos, yytext, AND) );
  <INITIAL>"andalso"	=> ( token(yypos, yytext, ANDALSO) );
  <INITIAL>"as"		=> ( token(yypos, yytext, AS) );
  <INITIAL>"case"	=> ( token(yypos, yytext, CASE) );
  <INITIAL>"datatype"	=> ( token(yypos, yytext, DATATYPE) );
  <INITIAL>"do"		=> ( token(yypos, yytext, DO) );
  <INITIAL>"else"	=> ( token(yypos, yytext, ELSE) );
  <INITIAL>"end"	=> ( token(yypos, yytext, END) );
  <INITIAL>"eqtype"	=> ( token(yypos, yytext, EQTYPE) );
  <INITIAL>"exception"	=> ( token(yypos, yytext, EXCEPTION) );
  <INITIAL>"fn"		=> ( token(yypos, yytext, FN) );
  <INITIAL>"fun"	=> ( token(yypos, yytext, FUN) );
  <INITIAL>"functor"	=> ( token(yypos, yytext, FUNCTOR) );
  <INITIAL>"handle"	=> ( token(yypos, yytext, HANDLE) );
  <INITIAL>"if"		=> ( token(yypos, yytext, IF) );
  <INITIAL>"in"		=> ( token(yypos, yytext, IN) );
  <INITIAL>"include"	=> ( token(yypos, yytext, INCLUDE) );
  <INITIAL>"infix"	=> ( token(yypos, yytext, INFIX) );
  <INITIAL>"infixr"	=> ( token(yypos, yytext, INFIXR) );
  <INITIAL>"let"	=> ( token(yypos, yytext, LET) );
  <INITIAL>"local"	=> ( token(yypos, yytext, LOCAL) );
  <INITIAL>"nonfix"	=> ( token(yypos, yytext, NONFIX) );
  <INITIAL>"of"		=> ( token(yypos, yytext, OF) );
  <INITIAL>"op"		=> ( token(yypos, yytext, OP) );
  <INITIAL>"open"	=> ( token(yypos, yytext, OPEN) );
  <INITIAL>"orelse"	=> ( token(yypos, yytext, ORELSE) );
  <INITIAL>"raise"	=> ( token(yypos, yytext, RAISE) );
  <INITIAL>"rec"	=> ( token(yypos, yytext, REC) );
  <INITIAL>"sharing"	=> ( token(yypos, yytext, SHARING) );
  <INITIAL>"sig"	=> ( token(yypos, yytext, SIG) );
  <INITIAL>"signature"	=> ( token(yypos, yytext, SIGNATURE) );
  <INITIAL>"struct"	=> ( token(yypos, yytext, STRUCT) );
  <INITIAL>"structure"	=> ( token(yypos, yytext, STRUCTURE) );
  <INITIAL>"then"	=> ( token(yypos, yytext, THEN) );
  <INITIAL>"type"	=> ( token(yypos, yytext, TYPE) );
  <INITIAL>"val"	=> ( token(yypos, yytext, VAL) );
  <INITIAL>"where"	=> ( token(yypos, yytext, WHERE) );
  <INITIAL>"while"	=> ( token(yypos, yytext, WHILE) );
  <INITIAL>"with"	=> ( token(yypos, yytext, WITH) );
  <INITIAL>"withtype"	=> ( token(yypos, yytext, WITHTYPE) );

  <INITIAL>"0"		=> ( token(yypos, yytext, ZERO) );
  <INITIAL>[1-9]	=> ( tokenOf(yypos, yytext, DIGIT, toInt) );
  <INITIAL>{numericlab}	=> ( tokenOf(yypos, yytext, NUMERIC, toInt) );
  <INITIAL>{decint}	=> ( tokenOf(yypos, yytext, INT, toInt) );
  <INITIAL>{hexint}	=> ( tokenOf(yypos, yytext, HEXINT, toHexInt) );
  <INITIAL>{decword}	=> ( tokenOf(yypos, yytext, WORD, toWord) );
  <INITIAL>{hexword}	=> ( tokenOf(yypos, yytext, HEXWORD, toHexWord) );
  <INITIAL>{real}	=> ( tokenOf(yypos, yytext, REAL, toReal) );
  <INITIAL>{string}	=> ( tokenOf(yypos, yytext, STRING, toString) );
  <INITIAL>{char}	=> ( tokenOf(yypos, yytext, CHAR, toChar) );

  <INITIAL>{tyvar}	=> ( tokenOf(yypos, yytext, TYVAR, toId) );
  <INITIAL>{alphanumid}	=> ( tokenOf(yypos, yytext, ALPHA, toId) );
  <INITIAL>{symbolicid}	=> ( tokenOf(yypos, yytext, SYMBOL, toId) );
  <INITIAL>{longid}	=> ( tokenOf(yypos, yytext, LONGID, toLongId) );

  <INITIAL>"(*"		=> ( nesting := 1; YYBEGIN COMMENT; continue() );
  <COMMENT>"(*"		=> ( nesting := !nesting + 1; continue() );
  <COMMENT>"*)"		=> ( nesting := !nesting - 1;
			     if !nesting = 0 then YYBEGIN INITIAL else ();
			     continue() );
  <COMMENT>.		=> ( continue() );
  <COMMENT>"\n"		=> ( continue() );


  <INITIAL>"\""		=> ( error(yypos, yytext, "invalid string") );
  <INITIAL>.		=> ( invalid(yypos, yytext) );
