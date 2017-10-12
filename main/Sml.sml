(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML implementation main structure
 *)

structure Sml : SML =
struct
  (* Initial arguments *)

  type arg = (Infix.InfEnv * BindingBasis.Basis) * Basis.Basis * Program.State

  val J0          = InitialInfixEnv.J0
  val B0_BIND     = StaticBasis.toBindingBasis InitialStaticBasis.B0
  val B0_BIND'    = StaticBasis.toBindingBasis StaticLibrary.B0
  val B0_STAT     = InitialStaticBasis.B0
  val B0_DYN      = InitialDynamicBasis.B0
  val B0          = (B0_STAT, B0_DYN)
  val B0'         = (StaticLibrary.B0, DynamicLibrary.B0)
  val s0          = InitialDynamicBasis.s0
  val s0'         = DynamicLibrary.s0

  val initialArg  = ((J0, B0_BIND), B0, s0)
  val initialArg' = ((J0, B0_BIND'), B0', s0')


  (* Parsing only *)

  val checkProgram = SyntacticRestrictionsProgram.checkProgram

  fun parseArg (JB, B, s) = JB

  fun parse (J, B_BIND) (filenameOpt, source) =
      let
        val (J', program) = Parse.parse(J, source, filenameOpt)
        val B_BIND' = checkProgram(B_BIND, program)
      in
        PPProgram.ppProgram(TextIO.stdOut, 0, program);
        (J', B_BIND')
      end


  (* Parsing and elaboration *)

  fun elabArg ((J, B_BIND), (B_STAT, B_DYN), s) = (J, B_BIND, B_STAT)

  fun elab (J, B_BIND, B_STAT) (filenameOpt, source) =
      let
        val (J', program) = Parse.parse(J, source, filenameOpt)
        val B_BIND' = checkProgram(B_BIND, program)
        val B_STAT' = Program.elabProgram true (B_STAT, program)
      in
        (J', B_BIND', B_STAT')
      end


  (* Parsing and evaluation *)

  fun evalArg ((J, B_BIND), (B_STAT, B_DYN), s) = (J, B_BIND, B_DYN, s)

  fun eval (J, B_BIND, B_DYN, s) (filenameOpt, source) =
      let
        val (J', program) = Parse.parse(J, source, filenameOpt)
        val B_BIND' = checkProgram(B_BIND, program)
        val s'      = ref s
        val B_DYN'  = Program.evalProgram true ((s', B_DYN), program)
      in
        (J', B_BIND', B_DYN', !s')
      end


  (* Parsing, elaboration, and evaluation *)

  fun execArg arg = arg

  fun exec' echo ((J, B_BIND), B, s) (filenameOpt, source) =
      let
        val (J', program) = Parse.parse(J, source, filenameOpt)
        val B_BIND' = checkProgram(B_BIND, program)
        val s' = ref s
        val B' = Program.execProgram echo ((s', B), program)
      in
        ((J', B_BIND'), B', !s' )
      end

  val exec = exec' true


  (* Parsing, elaboration, and compilation to JavaScript *)

  fun compileJSArg ((J, B_BIND), (B_STAT, B_DYN), s) = (J, B_BIND, B_STAT)

  fun compileJS (J, B_BIND, B_STAT) (filenameOpt, source) =
      let
        val (J', program) = Parse.parse(J, source, filenameOpt)
        val B_BIND' = checkProgram(B_BIND, program)
        val B_STAT' = CompileJS.compileProgram TextIO.stdOut (B_STAT, program)
      in
        (J', B_BIND', B_STAT')
      end


  (* Process the `use' queue *)

  fun uses fromFile (process, arg, cont) =
      case Use.extract() of
        NONE      => arg
      | SOME name =>
           uses fromFile (process, fromFile (process, arg, cont) name, cont)


  (* Processing of strings *)

  exception Abort

  fun fromSource' fromUsedFile (process, arg, cont) (filenameOpt, source) =
      let
        val arg' = process arg (filenameOpt, source)
            handle Error.Error => if cont then arg else raise Abort
      in
        uses fromUsedFile (process, arg', cont)
      end

  fun fromString' fromUsedFile (process, arg, cont) source =
        fromSource' fromUsedFile (process, arg, cont) (NONE, source)

  fun fromInput' fromUsedFile (process, arg, cont) (n, source) =
        fromSource' fromUsedFile (process, arg, cont)
          (SOME("(input "^Int.toString n^")"), source)


  (* Processing of files *)

  fun fromFile' fromUsedFile (process, arg, cont) name =
      let
        val file   = TextIO.openIn name
        val source = TextIO.inputAll file ^ ";"
        val _      = TextIO.closeIn file
        val dir    = OS.FileSys.getDir()
        val dir'   =
            case OS.Path.dir name of "" => OS.Path.currentArc | dir' => dir'
      in
        OS.FileSys.chDir dir';
        fromSource' fromUsedFile (process, arg, cont) (SOME name, source)
        before OS.FileSys.chDir dir
      end
      handle IO.Io _ =>
        ( TextIO.output(TextIO.stdErr, name ^ ": read error\n");
          if cont then arg else raise Abort )

  fun fromFileLogged (process, arg, cont) name =
      ( TextIO.output(TextIO.stdErr, "[processing " ^ name ^ "]\n");
        TextIO.flushOut TextIO.stdErr;
        fromFile' fromFileLogged (process, arg, cont) name
      )

  fun fromString args    = fromString' fromFileLogged args
  fun fromInput args     = fromInput' fromFileLogged args
  fun fromFile args      = fromFile' fromFileLogged args
  fun fromFileQuiet args = fromFile' fromFileQuiet args


  (* Processing several files mentioned in a list file *)

  fun fromFiles (process, initialArg, cont) names =
      List.foldl
        (fn (name, arg) => fromFileLogged (process, arg, cont) name)
        initialArg names


  (* Session *)

  fun fromSession (process, initialArg, cont) =
      let
        fun inputLines prompt =
            let
              val _    = TextIO.output(TextIO.stdOut, prompt)
              val _    = TextIO.flushOut TextIO.stdOut
              val line = TextIO.inputLine TextIO.stdIn
            in
              case line of
                 NONE      => nil
               | SOME "\n" => "\n" :: inputLines "  "
               | SOME text =>
                 if String.sub(text, String.size text - 2) = #";" then
                   text :: nil
                 else
                   text :: inputLines "  "
            end

        fun loop(n, arg) =
            case inputLines "- " of
              nil   => ()
            | lines =>
                loop(n + 1,
                  fromInput (process, arg, cont) (n, String.concat lines))
      in
        loop(1, initialArg)
      end


  (* Install library *)

  val basisPath = ref(SOME "basis")
  val basisFile = "all.sml"

  fun loadLib() =
      case !basisPath of
        NONE =>
        ( TextIO.output(TextIO.stdErr, "[no standard basis library]\n");
          TextIO.flushOut TextIO.stdErr;
          initialArg'
        )
      | SOME path =>
        ( TextIO.output(TextIO.stdErr, "[loading standard basis library]\n");
          TextIO.flushOut TextIO.stdErr;
          fromFileQuiet (exec' false, initialArg', false)
            (OS.Path.joinDirFile{dir = path, file = basisFile})
        )

  val libRef = ref(NONE : arg option)

  fun lib() =
      case !libRef of
        SOME arg => arg
      | NONE => (libRef := SOME(loadLib()); lib())


  (* Plumbing *)

  fun processString (f, arg) s =
      ignore(fromString (f, arg(lib()), false) s) handle Abort => ()
  fun processFile (f, arg) s =
      ignore(fromFile (f, arg(lib()), false) s) handle Abort => ()
  fun processFiles (f, arg) s =
      ignore(fromFiles (f, arg(lib()), false) s) handle Abort => ()
  fun processSession (f, arg) () =
      fromSession(f, arg(lib() handle Abort => initialArg), true)

  val parseString      = processString(parse, parseArg)
  val elabString       = processString(elab, elabArg)
  val evalString       = processString(eval, evalArg)
  val execString       = processString(exec, execArg)
  val compileJSString  = processString(compileJS, compileJSArg)

  val parseFile        = processFile(parse, parseArg)
  val elabFile         = processFile(elab, elabArg)
  val evalFile         = processFile(eval, evalArg)
  val execFile         = processFile(exec, execArg)
  val compileJSFile    = processFile(compileJS, compileJSArg)

  val parseFiles       = processFiles(parse, parseArg)
  val elabFiles        = processFiles(elab, elabArg)
  val evalFiles        = processFiles(eval, evalArg)
  val execFiles        = processFiles(exec, execArg)
  val compileJSFiles   = processFiles(compileJS, compileJSArg)

  val parseSession     = processSession(parse, parseArg)
  val elabSession      = processSession(elab, elabArg)
  val evalSession      = processSession(eval, evalArg)
  val execSession      = processSession(exec, execArg)
  val compileJSSession = processSession(compileJS, compileJSArg)
end;
