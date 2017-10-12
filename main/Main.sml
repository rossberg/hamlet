(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML implementation stand-alone
 *)

structure Main : MAIN =
struct
    val version = "1.3.1/S5"

    fun usage() =
	( TextIO.output(TextIO.stdErr,
	    "Usage: hamlet [-<mode>] [file ...]\n\
	    \where <mode> is one of:\n\
	    \  h   help:       print this message\n\
	    \  p   parse mode: just parse input\n\
	    \  l   elab mode:  parse and elaborate\n\
	    \  v   eval mode:  parse and evaluate (no type checking!)\n\
	    \  x   exec mode:  parse, elaborate, and evaluate (default)\n"
	  )
	; TextIO.flushOut TextIO.stdErr
	; OS.Process.failure
	)

    fun start mode process =
	( TextIO.print("HaMLet " ^ version ^
		       " - To Become Or Not To Become Successor ML :-)\n")
	; case mode of NONE => () | SOME s => TextIO.print("[" ^ s ^ " mode]\n")
	; process()
	; TextIO.print "\n"
	; OS.Process.success
	)

    fun expand(name, names) =
	if String.size name = 0 orelse String.sub(name, 0) <> #"@" then
	    name::names
	else
	    let
		val file    = TextIO.openIn(String.extract(name, 1, NONE))
		val content = TextIO.inputAll file
		val _       = TextIO.closeIn file
	    in
		List.foldr expand names (String.tokens Char.isSpace content)
	    end

    fun run process names =
	( process (List.foldr expand [] names)
	; OS.Process.success
	)
	handle IO.Io _ =>
	( TextIO.output(TextIO.stdOut, "I/O error\n")
	; OS.Process.failure
	)

    fun main' ["-h"]        = ( usage() ; OS.Process.success )
      | main' ["-p"]        = start (SOME "Parsing") Sml.parseSession
      | main' ["-l"]        = start (SOME "Elaboration") Sml.elabSession
      | main' ["-v"]        = start (SOME "Evaluation") Sml.evalSession
      | main' ["-x"]        = start NONE Sml.execSession
      | main' []            = start NONE Sml.execSession
      | main' ("-p"::names) = run Sml.parseFiles names
      | main' ("-l"::names) = run Sml.elabFiles names
      | main' ("-v"::names) = run Sml.evalFiles names
      | main' ("-x"::names) = run Sml.execFiles names
      | main' names         = run Sml.execFiles names

    fun main() =
	let
	    val homeDir = case OS.Path.dir(CommandLine.name())
			    of ""  => OS.Path.currentArc
			     | dir => dir
	in
	    Sml.basisPath := OS.Path.joinDirFile{dir=homeDir, file="basis"};
	    OS.Process.exit(main'(CommandLine.arguments()))
	end
	handle exn =>
	( TextIO.output(TextIO.stdErr, "hamlet: unhandled internal exception " ^
				       (case exn
					  of Fail s =>
					     "Fail(\"" ^String.toString s^ "\")"
					   | _ => General.exnName exn) ^ "\n")
	; OS.Process.exit OS.Process.failure
	)
end;
