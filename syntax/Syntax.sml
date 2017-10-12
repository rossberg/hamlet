(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML abstract syntax
 *)

(*
 * Make syntax annotation operator globally infix. This is necessary because
 * it is used in Parser.grm, where fixity cannot be declared properly
 * (an infix directive placed in the header does not scope over the main rules). 
 *)
infix @@

structure AnnotationElab =
struct
  open Annotation

  fun elab A = hd(tl A)

  infix --> |->
  fun (x@@A) |-> a = (a --> elab A; (x, a))
end

structure AnnotationCore =
struct
  open AnnotationElab

  fun exhaustive A = hd(tl(tl A))

  local
    open StaticObjectsCore
  in
    type SCon_attr      = (TyName, nil) cons
    type Lab_attr       = nil
    type VId_attr       = (ValStr, nil) cons
    type TyCon_attr     = (TyStr, nil) cons
    type TyVar_attr     = nil
    type StrId_attr     = (Env, nil) cons
    type longVId_attr   = (ValStr, nil) cons
    type longTyCon_attr = (TyStr, nil) cons
    type longStrId_attr = (Env, nil) cons

    type AtExp_attr     = (Type, nil) cons
    type ExpRow_attr    = (RowType, nil) cons
    type Exp_attr       = (Type, nil) cons
    type Match_attr     = (Type, (Exhaustive, nil) cons) cons
    type Mrule_attr     = (Type, nil) cons
    type Dec_attr       = (Env, nil) cons
    type ValBind_attr   = (ValEnv, (Exhaustive, nil) cons) cons
    type TypBind_attr   = (TyEnv, nil) cons
    type DatBind_attr   = (ValEnv * TyEnv, nil) cons
    type ConBind_attr   = (ValEnv, nil) cons
    type ExBind_attr    = (ValEnv, nil) cons
    type AtPat_attr     = (ValEnv * Type, nil) cons
    type PatRow_attr    = (ValEnv * RowType, nil) cons
    type Pat_attr       = (ValEnv * Type, nil) cons
    type Ty_attr        = (Type, nil) cons
    type TyRow_attr     = (RowType, nil) cons
    type 'a seq_attr    = nil
  end
end

structure SyntaxCore = SyntaxCoreFn(AnnotationCore)

structure AnnotationModule =
struct
  open AnnotationElab

  structure Core = SyntaxCore

  local
    open StaticObjectsCore
    open StaticObjectsModule
  in
    type SigId_attr   = (Sig, nil) cons
    type FunId_attr   = (FunSig, nil) cons

    type StrExp_attr  = (Env, nil) cons
    type StrDec_attr  = (Env, nil) cons
    type StrBind_attr = (StrEnv, nil) cons
    type SigExp_attr  = (Env, nil) cons
    type SigDec_attr  = (SigEnv, nil) cons
    type SigBind_attr = (SigEnv, nil) cons
    type Spec_attr    = (Env, nil) cons
    type ValDesc_attr = (ValEnv, nil) cons
    type TypDesc_attr = (TyEnv, nil) cons
    type DatDesc_attr = (ValEnv * TyEnv, nil) cons
    type ConDesc_attr = (ValEnv, nil) cons
    type ExDesc_attr  = (ValEnv, nil) cons
    type StrDesc_attr = (StrEnv, nil) cons
    type FunDec_attr  = (FunEnv, nil) cons
    type FunBind_attr = (FunEnv, nil) cons
    type TopDec_attr  = (Basis, nil) cons
  end
end

structure SyntaxModule = SyntaxModuleFn(AnnotationModule)

structure AnnotationProgram =
struct
  open AnnotationElab

  structure Module = SyntaxModule

  local
    open StaticObjectsModule
  in
    type Program_attr = (Basis, nil) cons
  end
end

structure SyntaxProgram = SyntaxProgramFn(AnnotationProgram);
