(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML infix resolution
 *
 * Definition, Section 2.6
 *)

signature INFIX =
sig
    (* Import *)

    type Info		= GrammarCore.Info

    type Op		= GrammarCore.Op
    type VId		= GrammarCore.VId
    type longVId	= GrammarCore.longVId
    type Exp		= GrammarCore.Exp
    type Pat		= GrammarCore.Pat
    type AtExp		= GrammarCore.AtExp
    type AtPat		= GrammarCore.AtPat


    (* Modifying fixity status *)

    datatype Assoc	= LEFT | RIGHT

    type InfStatus	= Assoc * int
    type InfEnv		= InfStatus VIdMap.map		(* [J] *)

    val empty :		InfEnv
    val assign :	InfEnv * VId list * InfStatus -> InfEnv
    val cancel :	InfEnv * VId list -> InfEnv


    (* Resolving phrases containing infixed identifiers *)

    val parseExp :	InfEnv * AtExp list -> Exp
    val parsePat :	InfEnv * AtPat list -> Pat
    val parseFmrule :	InfEnv * AtPat list -> Op * VId * AtPat list
end;
