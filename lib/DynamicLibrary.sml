(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library primitives
 *
 * Definition, Sections 6.2, 6.4 and Appendices C, D, and E; Standard Basis
 * Specification
 *
 * Notes:
 * - This defines all entities of the Standard Basis that are primitive in the
 *   sense that they cannot be implemented in the hosted language itself:
 *   - either because their behaviour cannot be implemented,
 *   - or because they have special types (ie. overloaded types).
 * - The initial dynamic basis B0 has to match StaticLibrary.B0.
 * - The initial basis only contains:
 *   - the vector type,
 *   - the overloaded functions,
 *   - the exceptions used by primitives,
 *   - the function `use'.
 *   Everything else can be implemented on top, as a self-hosted library.
 *   We piggyback the `use' function to allow the actual library implementation
 *   to gain access to the primitives. Use is given the unsafe type 'a -> 'b in
 *   the initial basis. Applying it to a record of type {b:string} will return
 *   the basic value denoted by the string b. The library implementation needs
 *   to annotate the result type properly to be type-safe. It also needs to
 *   restrict the type of `use' to string -> unit in its export environment so
 *   that user code cannot access the unsafe behaviour.
 * - Primitive constants are implemented as functions from unit.
 * - Currently, Real.toString is also primitive for simplicity.
 * - The official behaviour of `use' is implemented by putting all strings into
 *   a queue processed by the interpreter loop after evaluation.
 * - The dynamic semantics of the Definition does not really support the direct
 *   addition of new primitive library types - in general, that would require
 *   extending the set Val. Moreover, the APPLY function would need access
 *   to the state (eg. to implement arrays).
 *   But we can at least encode vectors by abusing the record representation.
 *   Arrays can then be implemented via vectors and references in the source
 *   language (making their implementation type transparent, however).
 *   I/O types can be implemented as indices into a stateful table.
 * - In order to approximate the correct semantics as close as possible in
 *   evaluation mode, we allow values of the default type of any overloading
 *   class to be implicitly coerced to other types of the same class.
 *   This way, we can deal with expressions like (Word8.toInt 0wff) in
 *   evaluation mode. Same for type dispatch in overloaded functions.
 * - Figure 27 of the Definition contains typos in the types of the comparison
 *   operators: their types should be numtxt * numtxt -> bool.
 * - We do not yet cover all required functionality if the Standard Basis.
 * - OS.SysErr currently is not in the environment (because its type contains
 *   the non-primitive option type).
 *)

structure DynamicLibrary : DYNAMIC_LIBRARY =
struct
  (* Import *)

  open DynamicObjectsCore
  open DynamicObjectsModule
  open IdsLibrary

  type TyName = StaticObjectsCore.TyName


  (* Dynamic objects for the Core *)

  (* Exception names *)

  val enChr       = ExName.exname vid_Chr
  val enDiv       = ExName.exname vid_Div
  val enDomain    = ExName.exname vid_Domain
  val enOverflow  = ExName.exname vid_Overflow
  val enSize      = ExName.exname vid_Size
  val enSubscript = ExName.exname vid_Subscript
  val enIo        = ExName.exname vid_Io
  val enSysErr    = ExName.exname vid_SysErr

  val ens =
      [enChr, enDiv, enDomain, enOverflow, enSize, enSubscript, enIo, enSysErr]
  val s0 =
      List.foldl (fn(en, s) => State.insertExName(s, en))
        InitialDynamicBasis.s0 ens


  (* Dynamic objects for Modules *)

  (* Dynamic basis *)

  val emptySE = StrIdMap.empty
  val emptyTE = TyConMap.empty
  val emptyVE = VIdMap.empty

  val TE_Word8 : TyEnv = TyConMap.fromList[(tyconWord8, emptyVE)]
  val VE_IO : ValEnv   = VIdMap.fromList[(vid_Io, (ExVal(ExName enIo), e))]

  val SE0 : StrEnv =
      StrIdMap.fromList[
        (stridWord8, Env(emptySE, TE_Word8, emptyVE)),
        (stridIO,    Env(emptySE, emptyTE,  VE_IO))
      ]

  val TE0 : TyEnv =
      TyConMap.fromList[
        (tyconVector,  VIdMap.empty)
      ]

  val VE0 : ValEnv =
      VIdMap.fromList[
        (vid_Chr,       (ExVal(ExName enChr), e)),
        (vid_Div,       (ExVal(ExName enDiv), e)),
        (vid_Domain,    (ExVal(ExName enDomain), e)),
        (vid_Overflow,  (ExVal(ExName enOverflow), e)),
        (vid_Size,      (ExVal(ExName enSize), e)),
        (vid_Subscript, (ExVal(ExName enSubscript), e)),
        (vidAbs,        (BasVal "abs", v)),
        (vidNeg,        (BasVal "~", v)),
        (vidPlus,       (BasVal "+", v)),
        (vidMinus,      (BasVal "-", v)),
        (vidTimes,      (BasVal "*", v)),
        (vidDiv,        (BasVal "div", v)),
        (vidMod,        (BasVal "mod", v)),
        (vidBy,         (BasVal "/", v)),
        (vidLess,       (BasVal "<", v)),
        (vidGreater,    (BasVal ">", v)),
        (vidLessEq,     (BasVal "<=", v)),
        (vidGreaterEq,  (BasVal ">=", v)),
        (vidUse,        (BasVal "use", v))
      ]

  val E0 = Env(SE0, TE0, VE0)
  val F0 = FunIdMap.empty
  val G0 = SigIdMap.empty

  val B0 = DynamicBasis.plus(InitialDynamicBasis.B0, (F0, G0, E0))


  (* Representation types for special values [Section 6.2] *)

  open LibrarySVal


  (* Value representation packing and unpacking *)

  exception TypeError of string

  fun toInt(SVal(SVal.INT(INT n))) = n
    | toInt _ = raise TypeError "int expected"

  fun toWord(SVal(SVal.WORD(WORD w))) = w
    | toWord _ = raise TypeError "word expected"

  fun toWord8(SVal(SVal.WORD(WORD8 w))) = w
    | toWord8(SVal(SVal.WORD(WORD w)))  = wordToWord8 w (* Implicit coercion *)
    | toWord8 _ = raise TypeError "Word8.word expected"

  fun toString(SVal(SVal.STRING(STRING s))) = s
    | toString _ = raise TypeError "string expected"

  fun toChar(SVal(SVal.CHAR(CHAR c))) = c
    | toChar _ = raise TypeError "char expected"

  fun toReal(SVal(SVal.REAL(REAL x))) = x
    | toReal _ = raise TypeError "real expected"

  fun toExn(ExVal en) = en
    | toExn _ = raise TypeError "exception expected"

  fun toUnit(Record r) =
        if LabMap.isEmpty r then () else raise TypeError "unit expected"
    | toUnit _ = raise TypeError "unit expected"

  fun toPair1 toX v =
      case Val.toPair v of
        SOME(v1, v2) => (toX v1, toX v2)
      | NONE => raise TypeError "pair expected"

  fun toPair2 (toX, toY) v =
      case Val.toPair v of
        SOME(v1, v2) => (toX v1, toY v2)
      | NONE => raise TypeError "pair expected"

  fun toList toX v =
      case Val.toList v of
        SOME vs => List.map toX vs
      | NONE => raise TypeError "list expected"


  fun fromInt n    = SVal(SVal.INT(INT n))
  fun fromWord w   = SVal(SVal.WORD(WORD w))
  fun fromWord8 w  = SVal(SVal.WORD(WORD8 w))
  fun fromString s = SVal(SVal.STRING(STRING s))
  fun fromChar c   = SVal(SVal.CHAR(CHAR c))
  fun fromReal x   = SVal(SVal.REAL(REAL x))
  fun fromBool b   = VId(VId.fromString(Bool.toString b))
  fun fromUnit()   = Record LabMap.empty

  fun fromPair (fromX, fromY) (x, y) = Val.fromPair(fromX x, fromY y)

  fun fromList fromX xs = Val.fromList(List.map fromX xs)

  fun fromOption fromX NONE     = VId(VId.fromString "NONE")
    | fromOption fromX (SOME x) = VIdVal(VId.fromString "SOME", fromX x)


  (* Vectors encoded as records *)

  val fromVector = Record

  fun toVector(Record r) = r
    | toVector _         = raise TypeError "vector expected"

  val vectorLength = LabMap.numItems
  val vectorMaxLen = Option.getOpt(Int.maxInt, Vector.maxLen)

  fun vectorFromList vs =
      let
        val labs = List.tabulate(List.length vs, Lab.fromInt)
      in
        LabMap.fromList(ListPair.zipEq(labs, vs))
      end

  fun vectorSub(r, n) =
      case LabMap.find(r, Lab.fromInt n) of
        SOME v => v
      | NONE   => raise Subscript


  (* Tables for mapping streams *)

  structure IntMap = FinMapFn(type ord_key = int; val compare = Int.compare)

  val ixCounter  = ref 0
  val instreams  = ref(IntMap.empty : TextIO.instream IntMap.map)
  val outstreams = ref(IntMap.empty : TextIO.outstream IntMap.map)

  fun openIn is =
      let
        val ix = !ixCounter
      in
        ixCounter := ix + 1;
        instreams := IntMap.insert(!instreams, ix, is);
        ix
      end

  fun openOut os =
      let
        val ix = !ixCounter
      in
        ixCounter := ix + 1;
        outstreams := IntMap.insert(!outstreams, ix, os);
        ix
      end

  fun closeIn ix  = instreams := IntMap.delete(!instreams, ix)
  fun closeOut ix = outstreams := IntMap.delete(!outstreams, ix)

  val stdIn  = openIn TextIO.stdIn
  val stdOut = openOut TextIO.stdOut
  val stdErr = openOut TextIO.stdErr

  fun toInstream v = valOf(IntMap.find(!instreams, toInt v))
  fun toOutstream v = valOf(IntMap.find(!outstreams, toInt v))
  fun fromInstream s = fromInt(openIn s)
  fun fromOutstream s = fromInt(openOut s)

  fun fromIoArg{name, function, cause} =
      let
        (* Dummy exception *)
        val en = ExName.exname(VId.fromString(General.exnName cause))
        val r =
            LabMap.fromList[
              (Lab.fromString "name",     fromString name),
              (Lab.fromString "function", fromString function),
              (Lab.fromString "cause",    ExVal(ExName en))
            ]
      in
        Record r
      end

  fun fromSysErrArg(s, eo) =
      let
        val r =
            LabMap.fromList[
              (Lab.fromInt 1, fromString s),
              (Lab.fromInt 2, fromOption fromInt NONE)
            ]
      in
        Record r
      end


  (* Dynamic type dispatch *)

  fun realint1 (fInt, fReal) v =
      case v of
        SVal(SVal.INT(INT n))   => SVal(SVal.INT(INT(fInt n)))
      | SVal(SVal.REAL(REAL x)) => SVal(SVal.REAL(REAL(fReal x)))
      | _ => raise TypeError "value of class RealInt expected"

  fun num1 (fInt, fWord, fWord8, fReal) v =
      case v of
        SVal(SVal.WORD(WORD w))  => SVal(SVal.WORD(WORD(fWord w)))
      | SVal(SVal.WORD(WORD8 w)) => SVal(SVal.WORD(WORD8(fWord8 w)))
      | _ =>
          realint1 (fInt, fReal) v handle TypeError _ =>
            raise TypeError "values of class Num expected"

  fun Real2 (fReal) v =
      case Val.toPair v of
        SOME(SVal(SVal.REAL(REAL x1)), SVal(SVal.REAL(REAL x2))) =>
          SVal(SVal.REAL(REAL(fReal(x1, x2))))
      | _ => raise TypeError "value of class Real expected"

  fun wordint2 (fInt, fWord, fWord8) v =
      case Val.toPair v of
        SOME(SVal(SVal.INT(INT n1)), SVal(SVal.INT(INT n2))) =>
          SVal(SVal.INT(INT(fInt(n1, n2))))
      | SOME(SVal(SVal.WORD(WORD w1)), SVal(SVal.WORD(WORD w2))) =>
          SVal(SVal.WORD(WORD(fWord(w1, w2))))
      | SOME(SVal(SVal.WORD(WORD8 w1)), SVal(SVal.WORD(WORD8 w2))) =>
          SVal(SVal.WORD(WORD8(fWord8(w1, w2))))
      | SOME(SVal(SVal.WORD(WORD8 w1)), SVal(SVal.WORD(WORD w2))) =>
          (* Implicit coercion *)
          SVal(SVal.WORD(WORD8(fWord8(w1, wordToWord8 w2))))
      | SOME(SVal(SVal.WORD(WORD w1)), SVal(SVal.WORD(WORD8 w2))) =>
          (* Implicit coercion *)
          SVal(SVal.WORD(WORD8(fWord8(wordToWord8 w1, w2))))
      | _ => raise TypeError "values of class WordInt expected"

  fun num2 (fInt, fWord, fWord8, fReal) v =
      case Val.toPair v of
        SOME(SVal(SVal.REAL(REAL x1)), SVal(SVal.REAL(REAL x2))) =>
          SVal(SVal.REAL(REAL(fReal(x1, x2))))
      | _ =>
          wordint2 (fInt, fWord, fWord8) v handle TypeError _ =>
            raise TypeError "values of class Num expected"

  fun numtxt2 (fInt, fWord, fWord8, fReal, fChar, fString) v =
      case Val.toPair v of
        SOME(SVal(SVal.INT(INT n1)), SVal(SVal.INT(INT n2))) =>
          fromBool(fInt(n1, n2))
      | SOME(SVal(SVal.WORD(WORD w1)), SVal(SVal.WORD(WORD w2))) =>
          fromBool(fWord(w1, w2))
      | SOME(SVal(SVal.WORD(WORD8 w1)), SVal(SVal.WORD(WORD8 w2))) =>
          fromBool(fWord8(w1, w2))
      | SOME(SVal(SVal.WORD(WORD8 w1)), SVal(SVal.WORD(WORD w2))) =>
          (* Implicit coercion *)
          fromBool(fWord8(w1, wordToWord8 w2))
      | SOME(SVal(SVal.WORD(WORD w1)), SVal(SVal.WORD(WORD8 w2))) =>
          (* Implicit coercion *)
          fromBool(fWord8(wordToWord8 w1, w2))
      | SOME(SVal(SVal.REAL(REAL x1)), SVal(SVal.REAL(REAL x2))) =>
          fromBool(fReal(x1, x2))
      | SOME(SVal(SVal.CHAR(CHAR c1)), SVal(SVal.CHAR(CHAR c2))) =>
          fromBool(fChar(c1, c2))
      | SOME(SVal(SVal.STRING(STRING s1)), SVal(SVal.STRING(STRING s2))) =>
          fromBool(fString(s1, s2))
      | _ => raise TypeError "values of class NumTxt expected"


  (* The actual APPLY function [Section 6.4] *)

  fun packEx en  = raise Pack(ExName en, Source.nowhere)
  fun packIo arg = raise Pack(ExNameVal(enIo, fromIoArg arg), Source.nowhere)
  fun packSysErr arg =
        raise Pack(ExNameVal(enSysErr, fromSysErrArg arg), Source.nowhere)

  fun APPLY("abs", v) =
      ( realint1 (abs, abs) v handle Overflow => packEx enOverflow )
    | APPLY("~", v) =
      ( num1 (~, Word.~, Word8.~, ~) v handle Overflow => packEx enOverflow )
    | APPLY("+", v) =
      ( num2 (op+, op+, op+, op+) v handle Overflow => packEx enOverflow )
    | APPLY("-", v) =
      ( num2 (op-, op-, op-, op-) v handle Overflow => packEx enOverflow )
    | APPLY("*", v) =
      ( num2 (op*, op*, op*, op* ) v handle Overflow => packEx enOverflow )
    | APPLY("div", v) =
      ( wordint2 (op div, op div, op div) v
          handle Overflow => packEx enOverflow | Div => packEx enDiv )
    | APPLY("mod", v) =
      ( wordint2 (op mod, op mod, op mod) v handle Div => packEx enDiv )
    | APPLY("/", v) =
        Real2 (op/) v
    | APPLY("<", v) =
        numtxt2 (op<, op<, op<, op<, op<, op<) v
    | APPLY(">", v) =
        numtxt2 (op>, op>, op>, op>, op>, op>) v
    | APPLY("<=", v) =
        numtxt2 (op<=, op<=, op<=, op<=, op<=, op<=) v
    | APPLY(">=", v) =
        numtxt2 (op>=, op>=, op>=, op>=, op>=, op>=) v

    | APPLY("General.exnName", v) =
        fromString(ExName.toString(Val.exname(toExn v)))

    | APPLY("Char.ord", v) =
        fromInt(Char.ord(toChar v))
    | APPLY("Char.chr", v) =
      ( fromChar(Char.chr(toInt v)) handle Chr => packEx enChr )

    | APPLY("String.maxSize", v) =
        fromInt String.maxSize
    | APPLY("String.size", v) =
        fromInt(String.size(toString v))
    | APPLY("String.sub", v) =
      ( fromChar(String.sub(toPair2 (toString, toInt) v))
          handle Subscript => packEx enSubscript )
    | APPLY("String.str", v) =
        fromString(String.str(toChar v))
    | APPLY("String.^", v) =
      ( fromString(op^(toPair1 toString v)) handle Size => packEx enSize )

    | APPLY("Int.precision", v) =
        fromOption fromInt Int.precision
    | APPLY("Int.minInt", v) =
        fromOption fromInt Int.minInt
    | APPLY("Int.maxInt", v) =
        fromOption fromInt Int.maxInt
    | APPLY("Int.quot", v) =
      ( fromInt(Int.quot(toPair1 toInt v))
          handle Overflow => packEx enOverflow | Div => packEx enDiv )
    | APPLY("Int.rem", v) =
      ( fromInt(Int.rem(toPair1 toInt v)) handle Div => packEx enDiv )

    | APPLY("Word.wordSize", v) =
        fromInt Word.wordSize
    | APPLY("Word.toInt", v) =
      ( fromInt(Word.toInt(toWord v)) handle Overflow => packEx enOverflow )
    | APPLY("Word.toIntX", v) =
      ( fromInt(Word.toIntX(toWord v)) handle Overflow => packEx enOverflow )
    | APPLY("Word.fromInt", v) =
        fromWord(Word.fromInt(toInt v))
    | APPLY("Word.notb", v) =
        fromWord(Word.notb(toWord v))
    | APPLY("Word.orb", v) =
        fromWord(Word.orb(toPair1 toWord v))
    | APPLY("Word.xorb", v) =
        fromWord(Word.xorb(toPair1 toWord v))
    | APPLY("Word.andb", v) =
        fromWord(Word.andb(toPair1 toWord v))
    | APPLY("Word.<<", v) =
        fromWord(Word.<<(toPair1 toWord v))
    | APPLY("Word.>>", v) =
        fromWord(Word.>>(toPair1 toWord v))
    | APPLY("Word.~>>", v) =
        fromWord(Word.~>>(toPair1 toWord v))

    | APPLY("Word8.toLarge", v) =
        fromWord(word8ToWord(toWord8 v))
    | APPLY("Word8.toLargeX", v) =
        fromWord(word8ToWordX(toWord8 v))
    | APPLY("Word8.fromLarge", v) =
        fromWord8(wordToWord8(toWord v))
    | APPLY("Word8.toInt", v) =
      ( fromInt(Word8.toInt(toWord8 v)) handle Overflow => packEx enOverflow )
    | APPLY("Word8.toIntX", v) =
      ( fromInt(Word8.toIntX(toWord8 v)) handle Overflow => packEx enOverflow )
    | APPLY("Word8.fromInt", v) =
        fromWord8(Word8.fromInt(toInt v))
    | APPLY("Word8.notb", v) =
        fromWord8(Word8.notb(toWord8 v))
    | APPLY("Word8.orb", v) =
        fromWord8(Word8.orb(toPair1 toWord8 v))
    | APPLY("Word8.xorb", v) =
        fromWord8(Word8.xorb(toPair1 toWord8 v))
    | APPLY("Word8.andb", v) =
        fromWord8(Word8.andb(toPair1 toWord8 v))
    | APPLY("Word8.<<", v) =
        fromWord8(Word8.<<(toPair2 (toWord8, toWord) v))
    | APPLY("Word8.>>", v) =
        fromWord8(Word8.>>(toPair2 (toWord8, toWord) v))
    | APPLY("Word8.~>>", v) =
        fromWord8(Word8.~>>(toPair2 (toWord8, toWord)v))

(* Not supported by all implementations:
    | APPLY("Real.radix", v) =
        fromInt Real.radix
    | APPLY("Real.precision", v) =
        fromInt Real.precision
    | APPLY("Real.maxFinite", v) =
        fromReal Real.maxFinite
    | APPLY("Real.minPos", v) =
        fromReal Real.minPos
    | APPLY("Real.minNormalPos", v) =
        fromReal Real.minNormalPos
*)
    | APPLY("Real.==", v) =
        fromBool(Real.==(toPair1 toReal v))
    | APPLY("Real.?=", v) =
        fromBool(Real.?=(toPair1 toReal v))
    | APPLY("Real.isFinite", v) =
        fromBool(Real.isFinite(toReal v))
    | APPLY("Real.isNan", v) =
        fromBool(Real.isNan(toReal v))
(* Not supported by all implementations:
    | APPLY("Real.isNormal", v) =
        fromBool(Real.isNormal(toReal v))
*)
    | APPLY("Real.signBit", v) =
        fromBool(Real.signBit(toReal v))
    | APPLY("Real.copySign", v) =
        fromReal(Real.copySign(toPair1 toReal v))
(* Not supported by all implementations:
    | APPLY("Real.nextAfter", v) =
        fromReal(Real.nextAfter(toPair1 toReal v))
    | APPLY("Real.rem", v) =
        fromReal(Real.rem(toPair1 toReal v))
*)
    | APPLY("Real.checkFloat", v) =
      ( fromReal(Real.checkFloat(toReal v))
          handle Overflow => packEx enOverflow | Div => packEx enDiv )
(* Not supported by all implementations:
    | APPLY("Real.realFloor", v) =
        fromReal(Real.realFloor(toReal v))
    | APPLY("Real.realCeil", v) =
        fromReal(Real.realCeil(toReal v))
    | APPLY("Real.realTrunc", v) =
        fromReal(Real.realTrunc(toReal v))
    | APPLY("Real.realRound", v) =
        fromReal(Real.realRound(toReal v))
*)
    | APPLY("Real.floor", v) =
      ( fromInt(Real.floor(toReal v))
          handle Overflow => packEx enOverflow | Domain => packEx enDomain )
    | APPLY("Real.ceil", v) =
      ( fromInt(Real.ceil(toReal v))
          handle Overflow => packEx enOverflow | Domain => packEx enDomain )
    | APPLY("Real.trunc", v) =
      ( fromInt(Real.trunc(toReal v))
          handle Overflow => packEx enOverflow | Domain => packEx enDomain )
    | APPLY("Real.round", v) =
      ( fromInt(Real.round(toReal v))
          handle Overflow => packEx enOverflow | Domain => packEx enDomain )
    | APPLY("Real.fromInt", v) =
        fromReal(Real.fromInt(toInt v))
    | APPLY("Real.toString", v) =
        fromString(Real.toString(toReal v))

    | APPLY("Math.e", v) =
        fromReal Math.e
    | APPLY("Math.pi", v) =
        fromReal Math.pi
    | APPLY("Math.sqrt", v) =
        fromReal(Math.sqrt(toReal v))
    | APPLY("Math.sin", v) =
        fromReal(Math.sin(toReal v))
    | APPLY("Math.cos", v) =
        fromReal(Math.cos(toReal v))
    | APPLY("Math.tan", v) =
        fromReal(Math.tan(toReal v))
    | APPLY("Math.asin", v) =
        fromReal(Math.asin(toReal v))
    | APPLY("Math.acos", v) =
        fromReal(Math.acos(toReal v))
    | APPLY("Math.atan", v) =
        fromReal(Math.atan(toReal v))
    | APPLY("Math.atan2", v) =
        fromReal(Math.atan2(toPair1 toReal v))
    | APPLY("Math.exp", v) =
        fromReal(Math.exp(toReal v))
    | APPLY("Math.pow", v) =
        fromReal(Math.pow(toPair1 toReal v))
    | APPLY("Math.ln", v) =
        fromReal(Math.ln(toReal v))
    | APPLY("Math.log10", v) =
        fromReal(Math.log10(toReal v))
    | APPLY("Math.sinh", v) =
        fromReal(Math.sinh(toReal v))
    | APPLY("Math.cosh", v) =
        fromReal(Math.cosh(toReal v))
    | APPLY("Math.tanh", v) =
        fromReal(Math.tanh(toReal v))

    | APPLY("Vector.maxLen", v) =
        fromInt vectorMaxLen
    | APPLY("Vector.fromList", v) =
        fromVector(vectorFromList(toList (fn x => x) v))
    | APPLY("Vector.length", v) =
        fromInt(vectorLength(toVector v))
    | APPLY("Vector.sub", v) =
      ( vectorSub(toPair2 (toVector, toInt) v)
          handle Subscript => packEx enSubscript )

    | APPLY("CharVector.fromList", v) =
        fromString(CharVector.fromList(toList toChar v))

    | APPLY("TextIO.stdIn", v) =
        fromInt stdIn
    | APPLY("TextIO.stdOut", v) =
        fromInt stdOut
    | APPLY("TextIO.stdErr", v) =
        fromInt stdErr
    | APPLY("TextIO.openIn", v) =
      ( fromInstream(TextIO.openIn(toString v)) handle IO.Io x => packIo x )
    | APPLY("TextIO.openOut", v) =
      ( fromOutstream(TextIO.openOut(toString v)) handle IO.Io x => packIo x )
    | APPLY("TextIO.openAppend", v) =
      ( fromOutstream(TextIO.openAppend(toString v))
          handle IO.Io x => packIo x )
    | APPLY("TextIO.closeIn", v) =
      ( fromUnit(TextIO.closeIn(toInstream v)) handle IO.Io x => packIo x )
    | APPLY("TextIO.closeOut", v) =
      ( fromUnit(TextIO.closeOut(toOutstream v)) handle IO.Io x => packIo x )
    | APPLY("TextIO.input", v) =
      ( fromString(TextIO.input(toInstream v)) handle IO.Io x => packIo x )
    | APPLY("TextIO.input1", v) =
      ( fromOption fromChar(TextIO.input1(toInstream v))
          handle IO.Io x => packIo x )
    | APPLY("TextIO.inputN", v) =
      ( fromString(TextIO.inputN(toPair2 (toInstream, toInt) v))
          handle IO.Io x => packIo x )
    | APPLY("TextIO.inputAll", v) =
      ( fromString(TextIO.inputAll(toInstream v)) handle IO.Io x => packIo x )
    | APPLY("TextIO.inputLine", v) =
      ( fromOption fromString (TextIO.inputLine(toInstream v))
          handle IO.Io x => packIo x )
    | APPLY("TextIO.endOfStream", v) =
      ( fromBool(TextIO.endOfStream(toInstream v)) handle IO.Io x => packIo x )
    | APPLY("TextIO.output", v) =
      ( fromUnit(TextIO.output(toPair2 (toOutstream, toString) v))
          handle IO.Io x => packIo x )
    | APPLY("TextIO.output1", v) =
      ( fromUnit(TextIO.output1(toPair2 (toOutstream, toChar) v))
          handle IO.Io x => packIo x )
    | APPLY("TextIO.flushOut", v) =
      ( fromUnit(TextIO.flushOut(toOutstream v)) handle IO.Io x => packIo x )

    | APPLY("OS.FileSys.getDir", v) =
        fromString(OS.FileSys.getDir(toUnit v))
    | APPLY("OS.FileSys.chDir", v) =
      ( fromUnit(OS.FileSys.chDir(toString v))
          handle OS.SysErr x => packSysErr x )
    | APPLY("OS.FileSys.mkDir", v) =
      ( fromUnit(OS.FileSys.mkDir(toString v))
          handle OS.SysErr x => packSysErr x )
    | APPLY("OS.FileSys.rmDir", v) =
      ( fromUnit(OS.FileSys.rmDir(toString v))
          handle OS.SysErr x => packSysErr x )
    | APPLY("OS.FileSys.isDir", v) =
      ( fromBool(OS.FileSys.isDir(toString v))
          handle OS.SysErr x => packSysErr x)

    | APPLY("OS.Process.terminate", v) =
        OS.Process.terminate(
          if toInt v = 0 then OS.Process.success else OS.Process.failure)

    | APPLY("CommandLine.name", v) =
        fromString(CommandLine.name(toUnit v))
    | APPLY("CommandLine.arguments", v) =
        fromList fromString (CommandLine.arguments(toUnit v))

    | APPLY("use", v) =
        (case v of
          SVal(SVal.STRING(STRING s)) => 
            fromUnit(Use.enqueue s)
        | Record r =>
            (* We piggybag `use' to enable introduction of primitives. *)
            (case LabMap.find(r, Lab.fromString "b") of
              SOME(SVal(SVal.STRING(STRING s))) => BasVal s
            | _ => raise TypeError "invalid argument to `use'"
            )
        | _ => raise TypeError "string value expected"
        )

    | APPLY(b, v) =
        raise TypeError("unknown basic value `" ^ b ^ "'")
end;
