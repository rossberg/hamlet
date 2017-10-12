(* fun-infix.sml *)

(* Checks for parsing of infixed fun declarations *)

datatype t = C of int
datatype u = * of int * int
datatype v = / of t * t

fun x + y : int = x
fun (x) + (y) : int = x
fun (x + y) : int = x
fun ((x) + (y)) : int = x
fun ((x) + (y)) z : int = x
fun ((x) + (y)) (z) a b : int = x
fun (C x) + (C y) : int = x
fun ((C x) + (C y)) : int = x
fun (((C(x))) + ((C(y)))) a (z : int) b : int = x
fun (x*y) + (a*b) : int = x
fun ((x*y) + (a*b)) : int = x
fun ((x*y) + (a*b)) z : int = x
fun ((C x/C y) + (C a/C b)) : int = x
fun ((C x/C y) + (C a/C b)) z : int = x

(* Note that neither of the following has to be accepted:

fun C x + C y : int = x
fun C(x) + C(y) : int = x
fun (C x + C y) : int = x
fun (C(x) + C(y)) : int = x

*)
