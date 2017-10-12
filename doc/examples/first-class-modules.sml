(*****************************************************************************)
(*)
(*) First-class modules
(*)
(*****************************************************************************)

(*) First-class polymorphism

fun foo p x =
let
   open unpack p : (val f : 'a -> 'a list)
in
   f (f x)
end

val xss = foo (pack (fun f x = [x,x]) : (val f : 'a -> 'a list)) 33


(*) OO-style programming / existential types

signature SHAPE =
sig
   type shape
   val area : shape -> real
   val draw : shape -> unit
end

structure Rectangle =
struct
   type shape = {x : real, y : real, w : real, h : real}

   fun new xywh = xywh

   fun area{x, y, w, h} = w * h : real

   fun draw{x, y, w, h} =
      print("rect@(" ^ Real.toString x ^ "," ^ Real.toString y ^ ")\n")
end

structure Circle =
struct
   type shape = {x : real, y : real, r : real}

   fun new xyr = xyr

   fun area{x, y, r} = 2.0 * Math.pi * r * r

   fun draw{x, y, r} =
      print("circ@(" ^ Real.toString x ^ "," ^ Real.toString y ^ ")\n")
end

val rect1 = Rectangle.new{x=0.0, y=0.0, w=2.0, h=1.5}
val rect2 = Rectangle.new{x=2.0, y=1.5, w=0.5, h=3.2}
val circ1 = Circle.new{x=1.0, y=1.0, r=1.0}
val circ2 = Circle.new{x=0.0, y=1.0, r=0.7}

signature A_SHAPE = (include SHAPE; val it : shape)

val shapes = [pack (open Rectangle; val it = rect1) : A_SHAPE,
	      pack (open Circle; val it = circ1) : A_SHAPE,
	      pack (open Rectangle; val it = rect2) : A_SHAPE,
	      pack (open Circle; val it = circ2) : A_SHAPE]

fun area shape =
let
    structure Shape = unpack shape : A_SHAPE
in
    Shape.area Shape.it
end

fun draw shape =
let
    structure Shape = unpack shape : A_SHAPE
in
    Shape.draw Shape.it
end

do List.app draw shapes

val sumarea = List.foldl op+ 0.0 (List.map area shapes)
