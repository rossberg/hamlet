(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML infix resolution
 *
 * Definition, Section 2.6
 *)

signature INFIX =
sig
  (* Import *)

  type Op      = SyntaxCore.Op
  type VId     = SyntaxCore.VId
  type longVId = SyntaxCore.longVId
  type Exp     = SyntaxCore.Exp
  type Pat     = SyntaxCore.Pat
  type AtExp   = SyntaxCore.AtExp
  type AtPat   = SyntaxCore.AtPat


  (* Modifying fixity status *)

  datatype Assoc = LEFT | RIGHT

  type InfStatus = Assoc * int
  type InfEnv    = InfStatus VIdMap.map         (* [J] *)

  val empty  : InfEnv
  val assign : InfEnv * VId.Id list * InfStatus -> InfEnv
  val cancel : InfEnv * VId.Id list -> InfEnv

  (* Resolving phrases containing infixed identifiers *)

  val parseExp    : InfEnv * AtExp list -> Exp
  val parsePat    : InfEnv * AtPat list -> Pat
  val parseFmrule : InfEnv * AtPat list -> Op option * VId * AtPat list
end;
