(* sharing.sml *)

(* Checks treatment of sharing constraints. *)

signature S =
sig
  type t
  type s = t
  sharing type t = s
end;

signature T =   (* from SML/NJ doc *)
sig
  type s
  structure A :
  sig
    datatype d = D of s
    datatype t = K
  end
  sharing type s = A.t
end;

signature U =
sig
  datatype t = A | B
  datatype u = C
  sharing type t = u
end;

functor F (U : U) =
struct
  fun f U.A = 1
    | f U.B = 2
    | f U.C = 3
end;
