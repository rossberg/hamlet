(*
 * (c) Andreas Rossberg 2013
 *
 * Translation of core language into JavaScript.
 *)

signature JS_TRANSLATE_CORE =
sig
  (* Import *)

  type Dec  = SyntaxCore.Dec

  type name = JSSyntax.name
  type var  = JSSyntax.var
  type expr = JSSyntax.expr
  type stmt = JSSyntax.stmt

  type Env  = StaticObjectsCore.Env


  (* Export *)

  val packVar : expr option -> var -> name * expr
  val packEnv : expr option -> Env -> (name * expr) list
  val openEnv : expr -> Env -> stmt list
  val nestDec :
      Env * (stmt list -> stmt list) * (stmt list -> stmt list) -> stmt list

  val translateDec : Dec -> stmt list -> stmt list
end;
