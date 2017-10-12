(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Standard ML Basis Library primitives
 *
 * Definition, Sections 6.2, 6.4 and Appendices C, D and E
 *)

signature LIBRARY =
sig
    (* Import *)

    type TyName			= StaticObjectsCore.TyName
    type OverloadingClass	= StaticObjectsCore.OverloadingClass

    type Val			= DynamicObjectsCore.Val
    type BasVal			= DynamicObjectsCore.BasVal
    type State			= DynamicObjectsCore.State

    type StaticBasis		= StaticObjectsModule.Basis
    type DynamicBasis		= DynamicObjectsModule.Basis


    (* Hook file *)

    val file :			string

    (* Overloading classes [Section E.1] *)

    val Int :			OverloadingClass
    val Word :			OverloadingClass
    val Real :			OverloadingClass
    val String :		OverloadingClass
    val Char :			OverloadingClass

    val span :			TyName -> int

    (* Value class representation [Section 6.2] *)

    include LIBRARY_SVAL

    val intFromString :		SCon.base * string * TyName option -> IntSVal
    val wordFromString :	SCon.base * string * TyName option -> WordSVal
    val realFromString :	string * TyName option -> RealSVal
    val stringFromString :	string * TyName option -> StringSVal
    val charFromString :	string * TyName option -> CharSVal
				(* all xFromString can raise Overflow *)

    (* Initial basis [Appendices C and D] *)

    val B0_STAT : 		StaticBasis
    val B0_DYN :  		DynamicBasis
    val s0 :			State

    (* APPLY function [Section 6.4] *)

    exception TypeError		of string
    val APPLY : 		BasVal * Val -> Val
end;
