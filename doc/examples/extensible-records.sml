(*****************************************************************************)
(*)
(*) Extensible records
(*)
(*****************************************************************************)

(*) Optional arguments

val init =
{
    width = 20,
    height = 15,
    depth = 5,
    text = "",
    color = (0.0, 0.0, 0.0)
}

fun box{width, height, depth, text, color} =
    print "[Creating a box...]\n"

val box0 = box init
val box1 = box{init where text = "Hello World"}
val box2 = box{init where width = 40, height = 30, color = (1.0, 0.4, 0.0)}
