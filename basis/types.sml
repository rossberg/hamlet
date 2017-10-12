(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library top-level types
 *
 * Note:
 * - Vector, array, and substring types are declared with the modules.
 *)

type unit          = {}
type int           = int
type word          = word
type real          = real
type char          = char
type string        = string
type exn           = exn

datatype ref       = datatype ref
datatype bool      = datatype bool
datatype list      = datatype list

datatype 'a option = NONE | SOME of 'a 
datatype order     = LESS | EQUAL | GREATER;
