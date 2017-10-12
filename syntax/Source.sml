(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Helpers for handling source strings
 *)

structure Source : SOURCE =
struct
  type source = string
  type pos    = int * int
  type region = pos * pos
  type loc    = {file : string option, region : region}

  val nowhere = {file = NONE, region = ((0, 0), (0, 0))}

  fun after(line, col) = (line, col + 1)

  fun at'(r : region) = r
  fun left'(r : region) = (#1 r, after(#1 r))
  fun right'(r : region) = (#2 r, after(#2 r))
  fun over'(r1 : region, r2 : region) = (#1 r1, #2 r2)

  fun transform1 f (loc : loc) = {file = #file loc, region = f(#region loc)}
  fun transform2 f (loc1 : loc, loc2 : loc) = 
      {file = #file loc1, region = f(#region loc1, #region loc2)}

  val at = transform1 at'
  val left = transform1 left'
  val right = transform1 right'
  val over = transform2 over'

  fun comparePair compareOne ((x1, y1), (x2, y2)) =
      case compareOne(x1, x2) of
        EQUAL => compareOne(y1, y2)
      | order => order

  fun compare(loc1 : loc, loc2 : loc) =
      comparePair (comparePair Int.compare) (#region loc1, #region loc2)
end;
