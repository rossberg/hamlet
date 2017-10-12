(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML grammar
 *)

structure GrammarCore    = GrammarCoreFn(type Info = Source.info)

structure GrammarModule  = GrammarModuleFn(type Info = Source.info
					   structure Core = GrammarCore)

structure GrammarProgram = GrammarProgramFn(type Info = Source.info
					    structure Module = GrammarModule);
