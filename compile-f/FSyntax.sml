(*
 * (c) Andreas Rossberg 2013
 *
 * System F syntax.
 *)

structure FSyntax =
struct
    type var = string

    datatype index = Fst | Snd                     (* [i] *)

    datatype kind =                                (* [k] *)
        StarKind                                   (* * *)
      | ArrowKind                                  (* k -> k *)

    datatype typ =                                 (* t *)
        VarTyp   of var                            (* a *)
      | VoidTyp                                    (* 0 *)
      | UnitTyp                                    (* 1 *)
      | ArrowTyp of typ * typ                      (* t -> t *)
      | ProdTyp  of typ * typ                      (* t * t *)
      | SumTyp   of typ * typ                      (* t + t *)
      | UnivTyp  of var * kind * typ               (* forall a : k. t *)
      | ExistTyp of var * kind * typ               (* exists a : k. t *)
      | RecTyp   of var * kind * typ               (* mu a : k. t *)
      | ExnTyp                                     (* exn *)
      | TagTyp   of typ                            (* tag t *)
      | RefTyp   of typ                            (* ref t *)
      | FunTyp   of var * kind * typ               (* \ a : k. t *)
      | AppTyp   of typ * typ                      (* t t *)

    datatype value =                               (* [v] *)
        VarVal   of var                            (* x *)
      | UnitVal                                    (* () *)
      | FunVal   of var * typ * exp                (* \ x : t. e *)
      | ProdVal  of value * value                  (* <v, v> *)
      | SumVal   of index * value * typ            (* in_i v : t *)
      | RollVal  of value * typ                    (* roll v : t *)
      | GenVal   of var * kind * exp               (* \ a : k. e *)
      | PackVal  of typ * value * typ              (* <t, v> : t *)
      | ExnVal   of value * value                  (* exn v v *)

    and exp =                                      (* [e] *)
        ValExp   of value                          (* v *)
      | SeqExp   of exp * exp                      (* e ; e *)
      | AppExp   of exp * exp                      (* e e *)
      | ProjExp  of side * exp                     (* #i e *)
      | CaseExp  of exp * var * exp * var * exp    (* case e of x. e | x. e *)
      | UnrollExp of exp                           (* unroll e *)
      | InstExp  of exp * typ                      (* e t *)
      | UnpackExp of exp * var * var * exp         (* unpack e as <a, x>. e *)
      | TestExp  of exp * exp * var * exp * exp    (* test e is e x. e | e *)
      | ThrowExp of exp * tyo                      (* throw e : t *)
      | CatchExp of exp * var * exp                (* try e catch x. e *)
      | RefExp   of exp                            (* ref e *)
      | ReadExp  of exp                            (* !e *)
      | WriteExp of exp * exp                      (* e := e *)

    fun LetExp(exp1, typ, var, exp2) = AppExp(FunExp(var, typ, exp2), exp1)
end

structure VarMap = FiniteMapFn(type key = string; val compare = String.compare);
