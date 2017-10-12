(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *
 * Notes:
 * - Only valid for Unix-like APIs.
 * - There are several inconsistencies in the spec, particularly with respect
 *   to canonical paths.
 *)

structure OS =
struct
  structure Path : OS_PATH =
  struct
    infixr ^ @
    val op^ = String.^
    val op@ = List.@

    fun splitLast nil    = NONE
      | splitLast(x::xs) =
        case splitLast xs of
          NONE        => SOME(nil, x)
        | SOME(ys, y) => SOME(x::ys, y)

    exception Path
    exception InvalidArc

    val currentArc = "."
    val parentArc  = ".."

    fun isArcSep c = c = #"/"
    fun isExtSep c = c = #"."

    fun validVolume{isAbs, vol = ""} = true
      | validVolume _                = false

    fun fromString s =
        case String.fields isArcSep s of
          ""::nil  => {isAbs = false, vol = "", arcs = nil}
        | ""::arcs => {isAbs = true,  vol = "", arcs = arcs}
        | arcs     => {isAbs = false, vol = "", arcs = arcs}

    fun validArc arc =
        case fromString arc of
          {isAbs = false, vol = "", arcs} => List.length arcs <= 1
        | _                               => false

    fun toString{isAbs = false, vol, arcs = ""::_} = raise Path
      | toString{isAbs, vol, arcs} =
        if Bool.not(validVolume{isAbs = isAbs, vol = vol}) then
          raise Path
        else if Bool.not(List.all validArc arcs) then
          raise InvalidArc
        else
          vol ^ (if isAbs then "/" else "") ^ String.concatWith "/" arcs

    fun getVolume s = ""

    fun getParent s =
        if s = "/" then s else
        let
          val {isAbs, vol, arcs} = fromString s
        in
          case splitLast arcs of
            NONE          => ".."
          | SOME(_, "")   => s ^ ".."
          | SOME(_, ".")  => s ^ "."
          | SOME(_, "..") => s ^ "/.."
          | SOME(nil, _)  => toString{isAbs = isAbs, vol = vol, arcs = ["."]}
          | SOME(arcs, _) => toString{isAbs = isAbs, vol = vol, arcs = arcs}
        end

    fun splitDirFile s =
        let
          val {isAbs, vol, arcs} = fromString s
        in
          case splitLast arcs of
            NONE =>
            {dir = toString{isAbs = isAbs, vol = vol, arcs = nil}, file = ""}
          | SOME(arcs, arc) =>
            {dir = toString{isAbs = isAbs, vol = vol, arcs = arcs}, file = arc}
        end

    fun joinDirFile{dir, file} =
        let
          val {isAbs, vol, arcs} = fromString dir
          val arcs =
              if List.null arcs andalso file = "" then [] else arcs@[file]
        in
          toString{isAbs = isAbs, vol = vol, arcs = arcs}
        end

    fun splitArcBaseExt arc =
        let
          val (base, ext) =
              Substring.splitr (Bool.not o isExtSep) (Substring.full arc)
        in
          if Substring.isEmpty ext orelse Substring.size base <= 1 then
            NONE
          else
            SOME{
              base = Substring.string(Substring.trimr 1 base),
              ext  = Substring.string ext
            }
        end

    fun splitBaseExt s =
        let
          val {isAbs, vol, arcs} = fromString s
        in
          case splitLast arcs of
            NONE =>
            {base = toString{isAbs = isAbs, vol = vol, arcs = nil}, ext = NONE}
          | SOME(arcs, arc) =>
          case splitArcBaseExt arc of
            NONE => {base = s, ext = NONE}
          | SOME{base, ext} =>
            { base = toString{isAbs = isAbs, vol = vol, arcs = arcs@[base]},
              ext  = SOME ext }
        end

    fun joinBaseExt{base, ext = NONE}     = base
      | joinBaseExt{base, ext = SOME ""}  = base
      | joinBaseExt{base, ext = SOME arc} = base ^ "." ^ arc

    val dir  = #dir o splitDirFile
    val file = #file o splitDirFile
    val base = #base o splitBaseExt
    val ext  = #ext o splitBaseExt

    fun mkCanonicalArcs((*isAbs*) false, nil, nil) =
          "."::nil
      | mkCanonicalArcs(isAbs, nil, arcs') =
          List.rev arcs'
      | mkCanonicalArcs(isAbs, ""::arcs, arcs') =
          mkCanonicalArcs(isAbs, arcs, arcs')
      | mkCanonicalArcs(isAbs, "."::arcs, arcs') =
          mkCanonicalArcs(isAbs, arcs, arcs')
      | mkCanonicalArcs(true, ".."::arcs, nil) =
          mkCanonicalArcs(true, arcs, nil)
      | mkCanonicalArcs(isAbs, ".."::arcs, ".."::arcs') =
          mkCanonicalArcs(true, arcs, ".."::".."::arcs')
      | mkCanonicalArcs(isAbs, ".."::arcs, _::arcs') =
          mkCanonicalArcs(isAbs, arcs, arcs')
      | mkCanonicalArcs(isAbs, arc::arcs, arcs') =
          mkCanonicalArcs(isAbs, arcs, arc::arcs')

    fun mkCanonical s =
        let
          val {isAbs, vol, arcs} = fromString s
          val arcs' = mkCanonicalArcs(isAbs, arcs, nil)
        in
          toString{isAbs = isAbs, vol = vol, arcs = arcs'}
        end

    fun isCanonical s = s = mkCanonical s
    fun isAbsolute s  = #isAbs(fromString s)
    fun isRelative s  = Bool.not(isAbsolute s)
    fun isRoot s      = s = "/" orelse s = "/."

    fun concatArcs(arcs1, arcs2) =
        case splitLast arcs1 of
          NONE             => arcs2
        | SOME(arcs1', "") => arcs1' @ arcs2
        | SOME _           => arcs1 @ arcs2

    fun concat(s1, s2) =
        case (fromString s1, fromString s2) of
          (_, {isAbs = true, ...}) => raise Path
        | ({isAbs, vol = vol1, arcs = arcs1},
           {vol = vol2, arcs = arcs2, ...}) =>
          if vol2 = "" orelse vol1 = vol2 then
            toString{isAbs = isAbs, vol = vol1, arcs = concatArcs(arcs1, arcs2)}
          else raise Path

    fun mkAbsolute{path, relativeTo} =
        if isRelative relativeTo then
          raise Path
        else if isAbsolute path then
          path
        else
          mkCanonical(concat(relativeTo, path))

    fun stripCommonPrefix(x::xs, y::ys) =
        if x = y then stripCommonPrefix(xs, ys) else (x::xs, y::ys)
      | stripCommonPrefix(xs, ys) = (xs, ys)

    fun mkRelative{path, relativeTo} =
        let
          val abs = mkCanonical relativeTo
          val {isAbs = isAbs1, vol = vol1, arcs = arcs1} = fromString path
          val {isAbs = isAbs2, vol = vol2, arcs = arcs2} = fromString abs
          val arcs1 = case arcs1 of [""] => nil | _ => arcs1
        in
          if Bool.not isAbs2 then
            raise Path
          else if Bool.not isAbs1 then
            path
          else if Bool.not(vol1 = vol2) then
            raise Path
          else case stripCommonPrefix(arcs1, arcs2) of
            (nil, nil) => "."
          | (arcs1', arcs2') =>
              toString{isAbs = false, vol = vol1,
                arcs = List.map (fn _ => parentArc) arcs2' @ arcs1'}
        end

    fun fromUnixPath s = s
    fun toUnixPath s   = s
  end
end;
