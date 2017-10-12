(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Helpers for handling source strings
 *)

structure Source :> SOURCE =
struct
    type source = string
    type pos    = int * int
    type region = pos * pos
    type info   = {file : string option, region : region}

    exception Error of (int * int) * string

    val nowhere = {file = NONE, region = ((0,0), (0,0))}

    fun over'(r1 : region, r2 : region)    = (#1 r1, #2 r2)
    fun between'(r1 : region, r2 : region) = (#2 r1, #1 r2)

    fun transform f (i1 : info, i2 : info) = 
	{file = #file i1, region = f(#region i1, #region i2)}

    val over    = transform over'
    val between = transform between'

    fun comparePair compare1 ((x1,y1), (x2,y2)) =
	case compare1(x1, x2)
	  of EQUAL => compare1(y1, y2)
	   | order => order

    fun compare(i1 : info, i2 : info) =
	comparePair (comparePair Int.compare) (#region i1, #region i2)
end;
