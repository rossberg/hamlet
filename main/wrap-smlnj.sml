SMLofNJ.Internals.GC.messages false;

(* Ugliest hack to support built with old and new CM (SML/NJ >= 110.20) *)
CM.make(Unsafe.cast "sources.cm");

(* Force built-time evaluation of basis lib *)
(*Sml.parseString ";";*)

(* Export *)
SMLofNJ.exportFn ("hamlet-image", Main.main o ignore);
