use "fix/Word-smlsharp.sml";
use "fix/CommandLine-smlsharp.sml";
use "hamlet.sml";
use "main/MAIN-sig.sml";
use "main/Main.sml";
Sml.basisPath := OS.FileSys.getDir() ^ "/basis";
val _ = Main.main()




(* Unsuccessful attempts to hack around flushing bug in SML#...
let
    val stdout = TextIO.getOutstream TextIO.stdOut
in
    TextIO.StreamIO.setBufferMode(stdout, IO.NO_BUF);
    TextIO.setOutstream(TextIO.stdOut, stdout)
end;
structure TextIO =
struct
    open TextIO
    fun flushOut os =
    (TextIO.setOutstream(os, TextIO.getOutstream os); 
     let
        val os' = TextIO.getOutstream os
        val _ = TextIO.StreamIO.flushOut os' 
        val (writer, m) = TextIO.StreamIO.getWriter os'
        val TextPrimIO.WR{writeVec=SOME writeVec,...} = writer
        val _ = writeVec(CharVectorSlice.full "")
        val os'' = TextIO.StreamIO.mkOutstream(writer, m)
     in
        TextIO.setOutstream(os, os'')
     end)
end;
  val _ = TextIO.output(TextIO.stdOut, "$ ")
  val _ = TextIO.flushOut TextIO.stdOut
  val s = TextIO.inputLine TextIO.stdIn
*)
