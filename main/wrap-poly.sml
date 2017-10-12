use "hamlet.sml";
use "main/MAIN-sig.sml";
use "main/Main.sml";
(* Need to flush stdOut to avoid ghost prompts appearing on HaMLet startup *)
TextIO.flushOut TextIO.stdOut; PolyML.export("hamlet", Main.main);
