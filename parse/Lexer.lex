(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML lexical analysis
 *
 * Definition, Sections 2.1-2.5, 3.1
 * + RFC: Syntax fixes
 * + RFC: Line comments
 * + RFC: Extended literal syntax
 * + RFC: Views 
 * + RFC: Higher-order functors
 * + RFC: First-class modules
 *
 * Notes:
 *   Since all lexical classes must be disjoint:
 *   - There is no single class ID, use ALPHA|SYMBOL|STAR|EQUALS.
 *   - There is no class LAB, use ALPHA|SYMBOL|NUMERIC|DIGIT|STAR.
 *   - ID does not contain `=' and `*', those are EQUALS and STAR.
 *   - LONGID does not contain unqualified ids (but allows for `=' and `*').
 *   - INT does not contain positive decimal integers without leading 0,
 *     and single DIGIT integers, those are in NUMERIC, DIGIT, and ZERO.
 *   - NUMERIC does not contain single digit numbers, those are in DIGIT.
 *   - DIGIT does not contain 0, that is ZERO.
 *
 *   The lexer uses a global variable to recognise nested comments, so it is
 *   not reentrant.
 *)


    open Tokens


    (* Types to match structure LEXER.UserDeclaration *)

    type ('a,'b) token = ('a,'b) Tokens.token
    type pos           = int
    type svalue        = Tokens.svalue
    type lexresult     = (svalue, pos) token



    (* Handling nested comments *)

    val nesting = ref 0		(* non-reentrant side-effect way :-P *)


    fun eof() =
	if !nesting = 0 then
	    Tokens.EOF(~1, ~1)
	else
	    raise Source.Error((0,0), "unclosed comment")



    (* Some helpers to create tokens *)

    open Tokens


    fun toLRPos(yypos, yytext) =
	let
	    val yypos = yypos - 2	(* bug in ML-Lex... *)
	in
	    (yypos, yypos + String.size yytext)
	end

    fun token(TOKEN, yypos, yytext) =
        TOKEN(toLRPos(yypos, yytext))

    fun tokenOf(TOKEN, toVal, yypos, yytext) =
	let
	    val i as (l,r) = toLRPos(yypos, yytext)
	in
	    TOKEN(toVal yytext, l, r)
	end

    fun error(yypos, yytext, s) =
	    raise Source.Error(toLRPos(yypos,yytext), s)

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
	    fun split  []    = raise Fail "Lexer.toLongId: empty longid"
	      | split [x]    = ([],x)
	      | split(x::xs) = let val (ys,y) = split xs in (x::ys,y) end
	in
	    split(String.fields (fn c => c = #".") s)
	end


    (* Convert constants *)

    fun toInt s     = s
    fun toHexInt s  = if String.sub(s, 0) = #"~" then
                          "~" ^ String.substring(s, 3, String.size s-3)
                      else
                          String.substring(s, 2, String.size s-2)
    fun toBinInt s  = String.substring(s, 2, String.size s-2)
    fun toWord s    = s
    fun toHexWord s = String.substring(s, 3, String.size s-3)
    fun toBinWord s = String.substring(s, 3, String.size s-3)
    fun toReal s    = s
    fun toString s  = String.substring(s, 1, String.size s-2)
    fun toChar s    = String.substring(s, 2, String.size s-3)


%%


%header	( functor LexerFn(structure Tokens : Parser_TOKENS) );

%s COMMENT LCOMMENT;

%full

  formatting = [\ \t\n\011\012\013]+;
  letter     = [A-Za-z];
  symbol     = [-!%&$#+/:<=>?@\\~`|*^];
  digit      = [0-9];
  hexdigit   = [0-9a-fA-F];
  bindigit   = [0-1];

  posdecint  = {digit}(({digit}|"_")*{digit})?;
  poshexint  = "0x"({hexdigit}|"_")*{hexdigit};
  posbinint  = "0b"({bindigit}|"_")*{bindigit};
  negdecint  = "~"{posdecint};
  neghexint  = "~"{poshexint};
  negbinint  = "~"{posbinint};
  decint     = {posdecint} | {negdecint};
  hexint     = {poshexint} | {neghexint};
  binint     = {posbinint} | {negbinint};
  decword    = "0w"({digit}|"_")*{digit};
  hexword    = ("0wx"|"0xw")({hexdigit}|"_")*{hexdigit};
  binword    = ("0wb"|"0bw")({bindigit}|"_")*{bindigit};

  exp        = "E" | "e";
  real       = ({decint}"."({digit}|"_")*{digit}({digit}|"_")*({exp}{decint})?)
	       | ({decint}{exp}{decint});

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


  <INITIAL>"#"		=> ( token(HASH,      yypos, yytext) );
  <INITIAL>"("		=> ( token(LPAR,      yypos, yytext) );
  <INITIAL>")"		=> ( token(RPAR,      yypos, yytext) );
  <INITIAL>"*"		=> ( token(STAR,      yypos, yytext) );
  <INITIAL>","		=> ( token(COMMA,     yypos, yytext) );
  <INITIAL>"->"		=> ( token(ARROW,     yypos, yytext) );
  <INITIAL>"..."	=> ( token(DOTS,      yypos, yytext) );
  <INITIAL>":"		=> ( token(COLON,     yypos, yytext) );
  <INITIAL>":>"		=> ( token(SEAL,      yypos, yytext) );
  <INITIAL>";"		=> ( token(SEMICOLON, yypos, yytext) );
  <INITIAL>"="		=> ( token(EQUALS,    yypos, yytext) );
  <INITIAL>"=>"		=> ( token(DARROW,    yypos, yytext) );
  <INITIAL>"?"		=> ( token(QUEST,     yypos, yytext) );
  <INITIAL>"["		=> ( token(LBRACK,    yypos, yytext) );
  <INITIAL>"]"		=> ( token(RBRACK,    yypos, yytext) );
  <INITIAL>"_"		=> ( token(UNDERBAR,  yypos, yytext) );
  <INITIAL>"{"		=> ( token(LBRACE,    yypos, yytext) );
  <INITIAL>"|"		=> ( token(BAR,       yypos, yytext) );
  <INITIAL>"}"		=> ( token(RBRACE,    yypos, yytext) );

  <INITIAL>"abstype"	=> ( token(ABSTYPE,   yypos, yytext) );
  <INITIAL>"and"	=> ( token(AND,       yypos, yytext) );
  <INITIAL>"andalso"	=> ( token(ANDALSO,   yypos, yytext) );
  <INITIAL>"as"		=> ( token(AS,        yypos, yytext) );
  <INITIAL>"case"	=> ( token(CASE,      yypos, yytext) );
  <INITIAL>"datatype"	=> ( token(DATATYPE,  yypos, yytext) );
  <INITIAL>"do"		=> ( token(DO,        yypos, yytext) );
  <INITIAL>"else"	=> ( token(ELSE,      yypos, yytext) );
  <INITIAL>"end"	=> ( token(END,       yypos, yytext) );
  <INITIAL>"eqtype"	=> ( token(EQTYPE,    yypos, yytext) );
  <INITIAL>"exception"	=> ( token(EXCEPTION, yypos, yytext) );
  <INITIAL>"fct"	=> ( token(FCT,       yypos, yytext) );
  <INITIAL>"fn"		=> ( token(FN,        yypos, yytext) );
  <INITIAL>"fun"	=> ( token(FUN,       yypos, yytext) );
  <INITIAL>"functor"	=> ( token(FUNCTOR,   yypos, yytext) );
  <INITIAL>"handle"	=> ( token(HANDLE,    yypos, yytext) );
  <INITIAL>"if"		=> ( token(IF,        yypos, yytext) );
  <INITIAL>"in"		=> ( token(IN,        yypos, yytext) );
  <INITIAL>"include"	=> ( token(INCLUDE,   yypos, yytext) );
  <INITIAL>"infix"	=> ( token(INFIX,     yypos, yytext) );
  <INITIAL>"infixr"	=> ( token(INFIXR,    yypos, yytext) );
  <INITIAL>"let"	=> ( token(LET,       yypos, yytext) );
  <INITIAL>"local"	=> ( token(LOCAL,     yypos, yytext) );
  <INITIAL>"nonfix"	=> ( token(NONFIX,    yypos, yytext) );
  <INITIAL>"of"		=> ( token(OF,        yypos, yytext) );
  <INITIAL>"op"		=> ( token(OP,        yypos, yytext) );
  <INITIAL>"open"	=> ( token(OPEN,      yypos, yytext) );
  <INITIAL>"orelse"	=> ( token(ORELSE,    yypos, yytext) );
  <INITIAL>"pack"	=> ( token(PACK,      yypos, yytext) );
  <INITIAL>"raise"	=> ( token(RAISE,     yypos, yytext) );
  <INITIAL>"rec"	=> ( token(REC,       yypos, yytext) );
  <INITIAL>"sharing"	=> ( token(SHARING,   yypos, yytext) );
  <INITIAL>"sig"	=> ( token(SIG,       yypos, yytext) );
  <INITIAL>"signature"	=> ( token(SIGNATURE, yypos, yytext) );
  <INITIAL>"struct"	=> ( token(STRUCT,    yypos, yytext) );
  <INITIAL>"structure"	=> ( token(STRUCTURE, yypos, yytext) );
  <INITIAL>"then"	=> ( token(THEN,      yypos, yytext) );
  <INITIAL>"type"	=> ( token(TYPE,      yypos, yytext) );
  <INITIAL>"unpack"	=> ( token(UNPACK,    yypos, yytext) );
  <INITIAL>"val"	=> ( token(VAL,       yypos, yytext) );
  <INITIAL>"viewtype"	=> ( token(VIEWTYPE,  yypos, yytext) );
  <INITIAL>"where"	=> ( token(WHERE,     yypos, yytext) );
  <INITIAL>"while"	=> ( token(WHILE,     yypos, yytext) );
  <INITIAL>"with"	=> ( token(WITH,      yypos, yytext) );
  <INITIAL>"withtype"	=> ( token(WITHTYPE,  yypos, yytext) );

  <INITIAL>"0"		=> ( token  (ZERO,              yypos, yytext) );
  <INITIAL>[1-9]	=> ( tokenOf(DIGIT,   toInt,    yypos, yytext) );
  <INITIAL>{numericlab}	=> ( tokenOf(NUMERIC, toInt,    yypos, yytext) );
  <INITIAL>{decint}	=> ( tokenOf(INT,     toInt,    yypos, yytext) );
  <INITIAL>{hexint}	=> ( tokenOf(HEXINT,  toHexInt, yypos, yytext) );
  <INITIAL>{binint}	=> ( tokenOf(BININT,  toBinInt, yypos, yytext) );
  <INITIAL>{decword}	=> ( tokenOf(WORD,    toWord,   yypos, yytext) );
  <INITIAL>{hexword}	=> ( tokenOf(HEXWORD, toHexWord,yypos, yytext) );
  <INITIAL>{binword}	=> ( tokenOf(BINWORD, toBinWord,yypos, yytext) );
  <INITIAL>{real}	=> ( tokenOf(REAL,    toReal,   yypos, yytext) );
  <INITIAL>{string}	=> ( tokenOf(STRING,  toString, yypos, yytext) );
  <INITIAL>{char}	=> ( tokenOf(CHAR,    toChar,   yypos, yytext) );

  <INITIAL>{tyvar}	=> ( tokenOf(TYVAR,   toId,     yypos, yytext) );
  <INITIAL>{alphanumid}	=> ( tokenOf(ALPHA,   toId,     yypos, yytext) );
  <INITIAL>{symbolicid}	=> ( tokenOf(SYMBOL,  toId,     yypos, yytext) );
  <INITIAL>{longid}	=> ( tokenOf(LONGID,  toLongId, yypos, yytext) );

  <INITIAL>"(*)"        => ( YYBEGIN LCOMMENT ; continue () );
  <INITIAL>"(*"		=> ( nesting := 1 ; YYBEGIN COMMENT ; continue() );

  <LCOMMENT>.           => ( continue () );
  <LCOMMENT>"\n"        => ( YYBEGIN (if !nesting = 0 then INITIAL
				 		      else COMMENT) ;
			     continue () );

  <COMMENT>"(*)"        => ( YYBEGIN LCOMMENT ; continue () );
  <COMMENT>"(*"		=> ( nesting := !nesting+1 ; continue() );
  <COMMENT>"*)"		=> ( nesting := !nesting-1 ;
			     if !nesting = 0 then YYBEGIN INITIAL else () ;
			     continue() );
  <COMMENT>.		=> ( continue() );
  <COMMENT>"\n"		=> ( continue() );


  <INITIAL>"\""		=> ( error(yypos, yytext, "invalid string") );
  <INITIAL>.		=> ( invalid(yypos, yytext) );
