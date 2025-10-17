(* scon *)

(* Checks parsing of special constants. *)

(* Note that the (**)-marked expressions may raise a runtime error even
 * in HaMLet due to broken implementations of {Char,Real}.scan in the host
 * system. *)

#"\^C";
#"\a";
#"\u0000";                              (**)
#"\       \x\           \";
3.2121212121212121212121212121;
2e3;
0.00000000000e123213213123213123123;    (**)
0wxabcd;
0xAaAa;
