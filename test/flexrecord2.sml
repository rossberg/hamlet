(* flexrecord2.sml *)

(* Checks type inference for flexible records with large context. *)

(* Not supported by Hamlet, since probably not intended by Definition. *)

(* Examples by Stephen Weeks *)

(* flexrecord2 *)
val _ =
   let
     val g = #foo
     val _ = g {foo = 13}
     val _ = g {foo = "yes"}
   in
     ()
   end
(* flexrecord2 *)

(* flexrecord3 *)
val _ =
   let
     val g = #foo
     val _ = g {foo = 13, goo = 1.0}
     val _ = g {foo = "yes", goo = 1.0}
   in
     ()
   end
(* flexrecord3 *)

(* flexrecord4 *)
val _ =
   let
     val g = #foo
     val _ = g {foo = 13, goo = 1.0}
     val _ = g {foo = "yes", goo = false}
   in
     ()
   end;
(* flexrecord4 *)

(* flexrecord5 *)
val _ =
   let
     val g = #foo
     val f = fn x => g {foo = "yes", goo = x}
     val _ = g {foo = 13, goo = 1.0}
   in
     f 2; f true
   end;
(* flexrecord5 *)
