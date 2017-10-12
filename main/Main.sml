(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML implementation stand-alone
 *)

structure Main : MAIN =
struct
  val version = "2.0.0"

  fun usage() =
      ( TextIO.output(TextIO.stdErr,
          "Usage: hamlet [-b <basis-dir>] [-<mode>] [<file> ...]\n\
          \where <mode> is one of:\n\
          \  ?   help:         print this message\n\
          \  p   parse mode:   just parse input\n\
          \  l   elab mode:    parse and elaborate\n\
          \  v   eval mode:    parse and evaluate (no type checking!)\n\
          \  x   exec mode:    parse, elaborate, and evaluate (default)\n\
          \  j   compile mode: parse, elaborate, and compile to JavaScript\n"
        );
        TextIO.flushOut TextIO.stdErr
      )

  fun start mode process =
      ( TextIO.print(
          "HaMLet " ^ version ^ " - To Be Or Not To Be Standard ML\n");
        case mode of NONE => () | SOME s => TextIO.print("[" ^ s ^ " mode]\n");
        process();
        TextIO.print "\n";
        OS.Process.success
      )

  fun expand(name, names) =
      if String.size name = 0 orelse String.sub(name, 0) <> #"@" then
        name::names
      else
        let
          val file    = TextIO.openIn(String.extract(name, 1, NONE))
          val content = TextIO.inputAll file
        in
          TextIO.closeIn file;
          List.foldr expand names (String.tokens Char.isSpace content)
        end

  fun run process names =
      if List.exists (fn s => s = "" orelse String.sub(s, 0) = #"-") names then
        ( usage(); OS.Process.failure )
      else
        ( process (List.foldr expand [] names); OS.Process.success )
          handle IO.Io _ =>
            ( TextIO.output(TextIO.stdErr, "hamlet: I/O error\n");
              OS.Process.failure )

  fun basis x = Sml.basisPath := (if x = "-" then NONE else SOME x)

  fun main' []              = start NONE Sml.execSession
    | main' ["-p"]          = start (SOME "Parsing") Sml.parseSession
    | main' ["-l"]          = start (SOME "Elaboration") Sml.elabSession
    | main' ["-v"]          = start (SOME "Evaluation") Sml.evalSession
    | main' ["-x"]          = start NONE Sml.execSession
    | main' ["-j"]          = start (SOME "JS compilation") Sml.compileJSSession
    | main' ("-?"::_)       = ( usage(); OS.Process.success )
    | main' ("-b"::x::rest) = ( basis x; main' rest )
    | main' ("-p"::names)   = run Sml.parseFiles names
    | main' ("-l"::names)   = run Sml.elabFiles names
    | main' ("-v"::names)   = run Sml.evalFiles names
    | main' ("-x"::names)   = run Sml.execFiles names
    | main' ("-j"::names)   = run Sml.compileJSFiles names
    | main' names           = run Sml.execFiles names

  fun main() =
      let
        val cmdDir  = OS.Path.dir(CommandLine.name())
        val homeDir = if cmdDir = "" then OS.Path.currentArc else cmdDir
      in
        Sml.basisPath :=
          SOME(OS.Path.joinDirFile{dir = homeDir, file = "basis"});
        OS.Process.exit(main'(CommandLine.arguments()))
      end handle exn =>
        let
          val message = case exn of Fail s => " " ^ s | _ => ""
        in
           TextIO.output(TextIO.stdErr,
             "hamlet: unhandled internal exception " ^
               General.exnName exn ^ message ^ "\n");
           OS.Process.exit OS.Process.failure
        end
end;
