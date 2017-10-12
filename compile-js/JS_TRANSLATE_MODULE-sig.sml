(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of module language into JavaScript.
 *)

signature JS_TRANSLATE_MODULE =
sig
  (* Import *)

  type TopDec = SyntaxModule.TopDec

  type var    = JSSyntax.var
  type expr   = JSSyntax.expr
  type stmt   = JSSyntax.stmt

  type Basis  = StaticObjectsModule.Basis


  (* Export *)

  val packBasis : expr option -> Basis -> (string * expr) list
  val openBasis : expr -> Basis -> stmt list
  val nestTopDec :
      Basis * (stmt list -> stmt list) * (stmt list -> stmt list) -> stmt list

  val translateTopDec : TopDec -> stmt list -> stmt list
end;
