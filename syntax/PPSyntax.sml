(*
 * (c) Andreas Rossberg 2007-2013
 *
 * Auxiliaries for printing syntax
 *)

structure PPSyntax : PP_SYNTAX =
struct
  open TextIO

  fun ppIndent(out, i) = output(out, CharVector.tabulate(2*i, fn _ => #" "))
  fun ppBegin out = output(out, "(")
  fun ppEnd out = output(out, ")")

  fun ppAnnot(out, A) =
      let
        val {file, region = ((line1, col1), (line2, col2))} = Annotation.loc A
      in
        case file of
          NONE   => ()
        | SOME f => ( output(out, f); output(out, ":") );
        output(out, Int.toString line1);
        output(out, ".");
        output(out, Int.toString col1);
        output(out, "-");
        output(out, Int.toString line2);
        output(out, ".");
        output(out, Int.toString col2)
      end

  fun ppHead'(out, i, s, A_opt) =
      ( ppIndent(out, i);
        ppBegin out;
        output(out, s);
        case A_opt of
          NONE   => ()
        | SOME A => ( output(out, " "); ppAnnot(out, A) )
      )

  fun ppFoot'(out, i, A_opt) = ( ppEnd out; output(out, "\n") )

  fun ppHead(out, i, s, A) = ( ppHead'(out, i, s, SOME A); output(out, "\n") )
  fun ppFoot(out, i, A) = ( ppIndent(out, i); ppFoot'(out, i, SOME A) )
  fun ppHeadFoot(out, i, s, A) =
      ( ppHead'(out, i, s, SOME A); ppFoot'(out, i, SOME A) )

  fun ppAtom(out, i, s1, s2) =
      ( ppHead'(out, i, s1, NONE);
        output(out, " ");
        output(out, s2);
        ppFoot'(out, i, NONE) )

  fun ppElem(out, i, s, A, []) =
        ppHeadFoot(out, i, s, A)
    | ppElem(out, i, s, A, subs) =
      ( ppHead(out, i, s, A);
        List.app (fn pp => pp(out, i+1)) subs;
        ppFoot(out, i, A) )

  fun ppOpt ppX (out, i, NONE)   = ()
    | ppOpt ppX (out, i, SOME x) = ppX(out, i, x)

  fun sub ppX x (out, i) = ppX(out, i, x)
  fun subs ppX xs (out, i) = List.app (fn x => ppX(out, i, x)) xs
  fun subo ppX x_opt (out, i) = ppOpt ppX (out, i, x_opt)
end;
