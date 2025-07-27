(* where2.sml *)

(* Checks if equality and constructor consistency is enforced for realisations. *)

signature S1 = sig eqtype t end where type t = unit -> unit;
signature S2 = sig datatype t = T end where type t = int * int;
