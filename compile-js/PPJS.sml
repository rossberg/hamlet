(*
 * (c) Andreas Rossberg 2013
 *
 * Printer for JavaScript syntax
 *)

structure PPJS : PP_JS =
struct
  (* Import *)

  open JSSyntax
  open PrettyPrint
  open PPMisc

  infixr ^^ ^/^


  (* Operators *)

  fun ppUnop PosOp = "+"
    | ppUnop NegOp = "-"
    | ppUnop NotOp = "!"
    | ppUnop TypOp = "typeof"

  fun ppBinop AddOp       = "+"
    | ppBinop SubOp       = "-"
    | ppBinop MulOp       = "*"
    | ppBinop DivOp       = "/"
    | ppBinop ModOp       = "%"
    | ppBinop BitAndOp    = "&"
    | ppBinop BitXorOp    = "^"
    | ppBinop BitOrOp     = "|"
    | ppBinop EqOp        = "==="
    | ppBinop NeqOp       = "!=="
    | ppBinop InOp        = "in"
    | ppBinop InstOp      = "instanceof"
    | ppBinop LessOp      = "<"
    | ppBinop GreaterOp   = ">"
    | ppBinop LessEqOp    = "<="
    | ppBinop GreaterEqOp = ">="
    | ppBinop AndOp       = "&&"
    | ppBinop OrOp        = "||"
    | ppBinop AsnOp       = "="


  (* Precedences *)

  val topPrec    = 0
  val funPrec    = topPrec + 1
  val asnPrec    = funPrec + 1
  val ifPrec     = asnPrec + 1
  val orPrec     = ifPrec + 1
  val andPrec    = orPrec + 1
  val bitorPrec  = andPrec + 1
  val bitxorPrec = bitorPrec + 1
  val bitandPrec = bitxorPrec + 1
  val eqPrec     = bitandPrec + 1
  val relPrec    = eqPrec + 1
  val shiftPrec  = relPrec + 1
  val addPrec    = shiftPrec + 1
  val mulPrec    = addPrec + 1
  val unPrec     = mulPrec + 1
  val newPrec    = unPrec + 1
  val postPrec   = newPrec + 1
  val atomPrec   = postPrec + 1

  fun binopPrec AsnOp       = asnPrec
    | binopPrec OrOp        = orPrec
    | binopPrec AndOp       = andPrec
    | binopPrec BitOrOp     = bitorPrec
    | binopPrec BitXorOp    = bitxorPrec
    | binopPrec BitAndOp    = bitandPrec
    | binopPrec EqOp        = eqPrec
    | binopPrec NeqOp       = eqPrec
    | binopPrec InOp        = relPrec
    | binopPrec InstOp      = relPrec
    | binopPrec LessOp      = relPrec
    | binopPrec GreaterOp   = relPrec
    | binopPrec LessEqOp    = relPrec
    | binopPrec GreaterEqOp = relPrec
    | binopPrec AddOp       = addPrec
    | binopPrec SubOp       = addPrec
    | binopPrec MulOp       = mulPrec
    | binopPrec DivOp       = mulPrec
    | binopPrec ModOp       = mulPrec


  (* Expressions *)

  fun paren doc           = text "(" ^^ fbox doc ^^ text ")"
  fun brace doc           = text "{" ^^ fbox doc ^^ text "}"
  fun brack doc           = text "[" ^^ fbox doc ^^ text "]"
  fun parenAt p (p', doc) = if p' > p then paren doc else doc

  fun isIdChar c = Char.isAlphaNum c orelse c = #"_" orelse c = #"$"
  fun isId name =
        CharVector.all isIdChar name andalso
        not(Char.isDigit(CharVector.sub(name, 0)))
  fun isNum name =
        CharVector.all Char.isDigit name andalso
        (String.sub(name, 0) <> #"0" orelse String.size name = 1)

  fun ppVar var = text var
  fun ppName name =
        text(
          if isId name orelse isNum name then name
          else "'" ^ String.toCString name ^ "'"
        )

  fun ppProp(name, expr) = ppName name ^^ text ":" ^^ nest(break ^^ ppExpr expr)

  and ppExpr expr = ppExprAt topPrec expr
  and ppExprAt p (VarExpr var) =
        ppVar var
    | ppExprAt p (ThisExpr) =
        text "this"
    | ppExprAt p (UndefExpr) =
        parenAt unPrec (p, text "void 0")
    | ppExprAt p (BoolExpr b) =
        text(Bool.toString b)
    | ppExprAt p (NumExpr n) =
        text n
    | ppExprAt p (StringExpr s) =
        text("\"" ^ s ^ "\"")
    | ppExprAt p (FunExpr(vars, stmts)) =
        parenAt funPrec (p,
          text "function" ^^ paren(fbox(nest(ppCommaList ppVar vars))) ^^
          text " " ^^ abox(ppBlock stmts)
        )
    | ppExprAt p (CallExpr(expr, exprs)) =
        parenAt postPrec (p,
          ppExprAt postPrec expr ^^
          paren(abox(nest(ebreak ^^ ppCommaList ppExpr exprs) ^^ ebreak))
        )
    | ppExprAt p (NewExpr(expr, exprs)) =
        parenAt newPrec (p,
          text "new " ^^
          ppExprAt (newPrec + 1) expr ^^
          paren(abox(nest(ebreak ^^ ppCommaList ppExpr exprs) ^^ ebreak))
        )
    | ppExprAt p (ObjExpr(props)) =
        brace(abox(nest(ebreak ^^ fbox(ppCommaList ppProp props)) ^^ ebreak))
    | ppExprAt p (ArrExpr(expr_opts)) =
      let
        fun adjustTrailingHole []     = []
          | adjustTrailingHole [NONE] = [NONE, NONE]
          | adjustTrailingHole(x::xs) = x :: adjustTrailingHole xs
      in
        brack(
          abox(
            nest(
              ebreak ^^
              fbox(ppCommaList ppExprOpt (adjustTrailingHole expr_opts))
            ) ^^ ebreak
          )
        )
      end
    | ppExprAt p (DotExpr(expr, name)) =
        parenAt postPrec (p,
          ppExprAt postPrec expr ^^ 
          (if isId name then
            text "." ^^ ppName name
          else
            text "[" ^^ ppName name ^^ text "]"
          )
        )
    | ppExprAt p (IdxExpr(expr1, expr2)) =
        parenAt postPrec (p,
          ppExprAt postPrec expr1 ^^ text "[" ^^ ppExpr expr2 ^^ text "]"
        )
    | ppExprAt p (UnExpr(unop, expr)) =
        parenAt unPrec (p, text(ppUnop unop ^ " ") ^^ ppExprAt unPrec expr)
    | ppExprAt p (BinExpr(binop, expr1, expr2)) =
        parenAt (binopPrec binop) (p,
          ppExprAt (binopPrec binop) expr1 ^^
          nest(
            break ^^
            text(ppBinop binop) ^/^
            ppExprAt (binopPrec binop + 1) expr2
          )
        )
    | ppExprAt p (IfExpr(expr1, expr2, expr3)) =
        parenAt ifPrec (p,
          ppExprAt (ifPrec + 1) expr1 ^^
          nest(
            break ^^
            nest(hbox(text "?" ^^ break)) ^^ ppExprAt ifPrec expr2 ^/^
            nest(hbox(text ":" ^^ break)) ^^ ppExprAt ifPrec expr3
          )
        )

  and ppExprOpt NONE       = text ""
    | ppExprOpt(SOME expr) = ppExpr expr


  (* Statements *)

  and ppStmt(ExprStmt(expr)) =
        (case expr of
          ObjExpr _ =>  paren(ppExpr expr)
        | _         =>  ppExpr expr
        ) ^^ text ";"
    | ppStmt(RetStmt(expr)) =
        text "return " ^^ nest(ppExpr expr) ^^ text ";"
    | ppStmt(IfStmt(expr, stmt1, stmt2_opt)) =
        abox(
          text "if " ^^ paren(nest(ppExpr expr)) ^^ text " " ^^ ppStmt stmt1 ^^
          (case stmt2_opt of
            SOME stmt2 => text " else " ^^ ppStmt stmt2
          | NONE => empty
          )
        )
    | ppStmt(ThrowStmt(expr)) =
        text "throw " ^^ nest(ppExpr expr) ^^ text ";"
    | ppStmt(TryStmt(stmts1, var, stmts2)) =
        abox(
          text "try " ^^ ppBlock stmts1 ^^
          text " catch" ^^ paren(ppVar var) ^^ text " " ^^ ppBlock stmts2
        )
    | ppStmt(BlockStmt(stmts)) =
        abox(ppBlock stmts)
    | ppStmt(VarStmt(var, expr)) =
        text "var " ^^ ppVar var ^^ text " =" ^^
        abox(nest(break ^^ ppExpr expr ^^ text ";"))
    | ppStmt(FunStmt(var, vars, stmts)) =
        text "function " ^^ ppVar var ^^
        paren(fbox(nest(ppCommaList ppVar vars))) ^^
        text " " ^^ abox(ppBlock stmts)

  and ppBlock stmts =
        nest(text "{" ^/^ vbox(ppList ppStmt stmts)) ^/^ text "}"
end;
