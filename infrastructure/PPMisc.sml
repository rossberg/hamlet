(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML miscellaneous pretty printing helpers
 *)

structure PPMisc : PP_MISC =
struct
  (* Import *)

  open PrettyPrint

  infixr ^^ ^/^


  (* Some PP combinators *)

  val nest                    = nest 2

  fun paren doc               = text "(" ^^ fbox(below doc) ^^ text ")"
  fun brace doc               = text "{" ^^ fbox(below doc) ^^ text "}"
  fun brack doc               = text "[" ^^ fbox(below doc) ^^ text "]"
  fun comment doc             = text "(* " ^^ fbox(below doc) ^^ text " *)"

  fun parenAt p (p', doc)     = if p' > p then paren doc else doc

  fun ppList ppX []           = empty
    | ppList ppX (x::xs)      = ppX x ^/^ ppList ppX xs

  fun ppCommaList ppX []      = empty
    | ppCommaList ppX [x]     = ppX x
    | ppCommaList ppX (x::xs) = ppX x ^^ text "," ^/^ ppCommaList ppX xs

  fun ppStarList ppX []       = empty
    | ppStarList ppX [x]      = ppX x
    | ppStarList ppX (x::xs)  = hbox(ppX x ^/^ text "*") ^/^ ppStarList ppX xs

  fun ppSeqPrec ppXPrec n []  = empty
    | ppSeqPrec ppXPrec n [x] = ppXPrec n x
    | ppSeqPrec ppXPrec n xs  = paren(ppCommaList (ppXPrec 0) xs)

  fun ppSeq ppX               = ppSeqPrec (fn _ => ppX) 0
end;
