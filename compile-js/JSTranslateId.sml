(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of identifiers into JavaScript.
 *)

structure JSTranslateId : JS_TRANSLATE_ID =
struct
  (* Import *)

  type Lab        = SyntaxCore.Lab
  type VId        = SyntaxCore.VId
  type StrId      = SyntaxCore.StrId
  type FunId      = SyntaxModule.FunId
  type longVId    = SyntaxCore.longVId
  type longStrId  = SyntaxCore.longStrId

  type name       = JSSyntax.name
  type var        = JSSyntax.var
  type expr       = JSSyntax.expr

  datatype phrase = datatype Annotation.phrase

  structure JS    = JSSyntax


  (* Identifiers and labels *)

  val keywords =
      [
        "break", "case", "catch", "continue", "debugger", "default", "delete",
        "do", "else", "finally", "for", "function", "if", "in",
        "instanceof", "new", "null", "return", "switch", "this", "throw", "try",
        "typeof", "var", "void", "while", "with",
        "class", "const", "enum", "export", "extends", "import", "super",
        "implements", "interface", "let", "package", "private", "protected",
        "public", "static", "yield"
      ]

  fun escapeSym #"!" = "bang"
    | escapeSym #"%" = "percent"
    | escapeSym #"&" = "and"
    | escapeSym #"$" = "dollar"
    | escapeSym #"#" = "hash"
    | escapeSym #"+" = "plus"
    | escapeSym #"-" = "minus"
    | escapeSym #"/" = "slash"
    | escapeSym #":" = "colon"
    | escapeSym #"<" = "less"
    | escapeSym #"=" = "equal"
    | escapeSym #">" = "greater"
    | escapeSym #"?" = "question"
    | escapeSym #"@" = "at"
    | escapeSym #"\\" = "backslash"
    | escapeSym #"~" = "tilde"
    | escapeSym #"`" = "backtick"
    | escapeSym #"^" = "caret"
    | escapeSym #"|" = "bar"
    | escapeSym #"*" = "asterisk"
    | escapeSym c    = str c

  fun escapeSymbolic id =
        String.concatWith "_" ("" :: List.map escapeSym (String.explode id))
  val escapeAlphaNum = String.map (fn #"'" => #"$" | c => c)

  fun isAlphaNum c = Char.isAlphaNum c orelse c = #"_" orelse c = #"'"

  fun escape id =
      if List.exists (fn keyword => id = keyword) keywords then
        "_" ^ id
      else if CharVector.all isAlphaNum id then
        escapeAlphaNum id
      else
        escapeSymbolic id

  fun translateLab' lab =
      case Lab.toInt lab of
        NONE   => Lab.toString lab
      | SOME i => Int.toString(i - 1)

  fun translateVId' vid        = escape(VId.toString vid)
  fun translateStrId' strid    = "$" ^ escapeAlphaNum(StrId.toString strid)
  fun translateFunId' funid    = "$$" ^ escapeAlphaNum(FunId.toString funid)

  fun translateLab(lab@@_)     = translateLab' lab
  fun translateVId(vid@@_)     = translateVId' vid
  fun translateStrId(strid@@_) = translateStrId' strid
  fun translateFunId(funid@@_) = translateFunId' funid


  (* Long identifiers *)

  fun translateLongId (explode, translateId') (longid@@_) =
      case explode longid of
        ([], id) =>
          JS.VarExpr(translateId' id)
      | (strid::strids, id) =>
          JS.DotExpr(
            List.foldl
              (fn (strid, expr) => JS.DotExpr(expr, translateStrId' strid))
              (JS.VarExpr(translateStrId' strid)) strids,
            translateId' id
          )

  fun translateLongVId longvid =
        translateLongId (LongVId.explode, translateVId') longvid
  fun translateLongStrId longstrid =
        translateLongId (LongStrId.explode, translateStrId') longstrid
end;
