(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML values
 *
 * Definition, Sections 6.2, 6.3, and 6.4
 *
 * Note:
 *   - All value types are parameterised over the representation of function
 *     closures to break up the recursion between values and environments.
 *   - The basic values are just strings.
 *)

signature VAL =
sig
    (* Import *)

    type Val		= DynamicObjectsCore.Val
    type ExVal		= DynamicObjectsCore.ExVal
    type ExName		= DynamicObjectsCore.ExName


    (* Operations *)

    val equal :		Val * Val -> bool

    val exname :	ExVal -> ExName

    val toPair :	Val -> (Val * Val) option
    val toList :	Val -> Val list option
end;
