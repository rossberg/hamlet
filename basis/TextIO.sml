(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Note: Incomplete.
 *)

structure TextIO :> TEXT_IO =
struct
  structure StreamIO : TEXT_STREAM_IO =
  struct
    type vector   = CharVector.vector
    type elem     = Char.char
  end

  type vector     = StreamIO.vector
  type elem       = StreamIO.elem
  type instream   = int
  type outstream  = int

  val stdIn       = use{b = "TextIO.stdIn"} () : instream
  val stdOut      = use{b = "TextIO.stdOut"} () : outstream
  val stdErr      = use{b = "TextIO.stdErr"} () : outstream

  val openIn      = use{b = "TextIO.openIn"} : string -> instream
  val openOut     = use{b = "TextIO.openOut"} : string -> outstream
  val openAppend  = use{b = "TextIO.openAppend"} : string -> outstream
  val closeIn     = use{b = "TextIO.closeIn"} : instream -> unit
  val closeOut    = use{b = "TextIO.closeOut"} : outstream -> unit

  val input       = use{b = "TextIO.input"} : instream -> vector
  val input1      = use{b = "TextIO.input1"} : instream -> elem option
  val inputN      = use{b = "TextIO.inputN"} : instream * int -> vector
  val inputAll    = use{b = "TextIO.inputAll"} : instream -> vector
  val inputLine   = use{b = "TextIO.inputLine"} : instream -> string option
  val endOfStream = use{b = "TextIO.endOfStream"} : instream -> bool

  val output      = use{b = "TextIO.output"} : outstream * vector -> unit
  val output1     = use{b = "TextIO.output1"} : outstream * elem -> unit
  val flushOut    = use{b = "TextIO.flushOut"} : outstream -> unit

  fun print s     = (output(stdOut, s); flushOut stdOut)

  fun outputSubstr(os, ss) = output(os, Substring.string ss)

(*
  fun scanStream scanFn strm  =
      let
        val instrm = getInstream strm
      in
        case (scanFn StreamIO.input1 instrm) of
          NONE => NONE
        | SOME(v, instrm') => ( setInstream (strm, instrm'); SOME v )
      end
*)
end;
