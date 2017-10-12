(*
 * (c) Andreas Rossberg 2013
 *
 * JavaScript syntax.
 *)

structure JSSyntax =
struct
  type name = string
  type var  = string

  datatype unop =
      PosOp
    | NegOp
    | NotOp
    | TypOp

  datatype binop =
      AddOp
    | SubOp
    | MulOp
    | DivOp
    | ModOp
    | BitOrOp
    | BitXorOp
    | BitAndOp
    | EqOp
    | NeqOp
    | InOp
    | InstOp
    | LessOp
    | GreaterOp
    | LessEqOp
    | GreaterEqOp
    | AndOp
    | OrOp
    | AsnOp

  datatype expr =
      VarExpr  of var                            (* x *)
    | ThisExpr                                   (* this *)
    | UndefExpr                                  (* undefined *)
    | BoolExpr of bool                           (* false, true *)
    | NumExpr  of string                         (* n, n.n *)
    | StringExpr of string                       (* "..." *)
    | FunExpr  of var list * stmt list           (* function(x1,...,xN) {ss} *)
    | CallExpr of expr * expr list               (* e0(e1,...,eN) *)
    | NewExpr  of expr * expr list               (* new e0(e1,...,eN) *)
    | ObjExpr  of (name * expr) list             (* {p1: e1,...,pN: eN} *)
    | ArrExpr  of expr option list               (* [e1,...,eN] *)
    | DotExpr  of expr * name                    (* e.x *)
    | IdxExpr  of expr * expr                    (* e1[e2] *)
    | UnExpr   of unop * expr                    (* @e *)
    | BinExpr  of binop * expr * expr            (* e1 @ e2 *)
    | IfExpr   of expr * expr * expr             (* e1 ? e2 : e3 *)

  and stmt =
      ExprStmt  of expr                          (* e *)
    | RetStmt   of expr                          (* return e *)
    | IfStmt    of expr * stmt * stmt option     (* if (e) s1 [else s2] *)
    | ThrowStmt of expr                          (* throw e *)
    | TryStmt   of stmt list * var * stmt list   (* try {ss1} catch (x) {ss2} *)
    | BlockStmt of stmt list                     (* {ss} *)
    | VarStmt   of var * expr                    (* var x = e *)
    | FunStmt   of var * var list * stmt list    (* function x(x1,...,xN){ss} *)

  fun DoExpr(stmts) = CallExpr(FunExpr([], stmts), [])
  fun AsnStmt(expr1, expr2) = ExprStmt(BinExpr(AsnOp, expr1, expr2))

  val inventVar =
      let val i = ref 0 in fn() => (i := !i + 1; "_x" ^ Int.toString(!i)) end
end;
