(*) Examples from:
(*) Phil Wadler
(*) Views: a way for pattern matching to cohabit with data abstraction
(*) 14th ACM Symposium on Principles of Programming Languages, 1987

(*) Numbers

viewtype peano = int as Zero | Succ of int
with
   fun to 0            = Zero
     | to n if (n > 0) = Succ(n-1)
     | to n            = raise Domain
   fun from Zero       = 0
     | from(Succ n)    = n+1
end

fun fib Zero          = Zero
  | fib(Succ Zero)    = Succ Zero
  | fib(Succ(Succ n)) = fib n + fib(Succ n)

viewtype evenodd = int as Zero | Even of int | Odd of int
with
   fun to 0                  = Zero
     | to n if (n mod 2 = 0) = Even(n div 2)
     | to n                  = Odd((n-1) div 2)
   fun from Zero             = 0
     | from(Even n)          = 2*n
     | from(Odd n)           = 2*n + 1
end

fun pow(x, Zero)   = 1
  | pow(x, Even n) = pow(x*x, n)
  | pow(x, Odd n)  = x*pow(x*x, n)

val 1024 = pow(2,10)


(*) Complex numbers

signature COMPLEX =
sig
   type complex
   viewtype cart = complex as Cart of real * real
   viewtype pole = complex as Pole of real * real
   val add : complex * complex -> complex
   val mul : complex * complex -> complex
end

structure Complex :> COMPLEX =
struct
   datatype complex = Cart of real * real
   datatype cart = datatype complex
   viewtype pole = complex as Pole of real * real
   with
      open Math
      fun to(Cart(x,y))   = Pole(sqrt(x*x + y*y), atan2(x,y))
      fun from(Pole(r,t)) = Cart(r*cos(t), r*sin(t))
   end
   fun add(Cart(x1,y1), Cart(x2,y2)) = Cart(x1 + x2, y1 + y2)
   fun mul(Pole(r1,t1), Pole(r2,t2)) = Pole(r1 * r2, t1 + t2)
end

local open Complex in
   val i  = Cart(0.0,1.0)
   val m1 = mul(i,i)
end


(*) Lists

infix 5 :::
viewtype 'a snoc_list = 'a list as Nil | op::: of 'a list * 'a
with
   fun to nil       = Nil
     | to(x::xs)    = case to xs of Nil      => nil:::x
				  | xs':::x' => (x::xs'):::x'
   fun from Nil     = nil
     | from(xs:::x) = case xs of nil     => x::nil
			       | x'::xs' => x'::from(xs':::x)
end

fun last(xs:::x)     = x
fun rotleft(x::xs)   = xs:::x
fun rotright(xs:::x) = x::xs

val [2,3,4,1] = rotleft[1,2,3,4]
val [4,1,2,3] = rotright[1,2,3,4]

viewtype ('a,'b) zip_list = ('a * 'b) list as Zip of 'a list * 'b list
with
   fun to nil         = Zip(nil, nil)
     | to((x,y)::xys) = let val Zip(xs,ys) = to xys in Zip(x::xs, y::ys) end
   fun from(Zip(nil,nil))     = nil
     | from(Zip(x::xs,y::ys)) = (x,y)::from(Zip(xs,ys))
     | from _                 = raise Domain
end


(*) Trees

datatype 'a tree = Leaf of 'a | Branch of 'a tree * 'a tree
viewtype 'a spine = 'a tree as Spine of 'a * 'a tree list
with
   fun to(Leaf x)            = Spine(x,Nil)
     | to(Branch(l,r))       = let val Spine(x,ts) = to l in Spine(x,ts:::r) end
   fun from(Spine(x,Nil))    = Leaf x
     | from(Spine(x,ts:::t)) = Branch(from(Spine(x,ts)), t)
end

val Spine(f, [Spine(a,[]), Spine(b,[])]) =
    Branch(Branch(Leaf "f", Leaf "a"), Leaf "b")


(*) Miscellaneous

viewtype 'a as_view = 'a as As of 'a * 'a
with
   fun to x          = As(x,x)
   fun from(As(x,_)) = x
end

viewtype is_even = int as IsEven of int | IsOdd of int
with
   fun to n if (n mod 2 = 0)           = IsEven n
     | to n                            = IsOdd n
   fun from(IsEven n) if (n mod 2 = 0) = n
     | from(IsOdd n)  if (n mod 2 = 1) = n
     | from _                          = raise Domain
end

val evens = map (fn IsEven n => n | IsOdd n => n+1) [1,2,3,4,5]


(*) Examples from:
(*) Chris Okasaki
(*) Views for Standard ML
(*) Workshop on ML, 1998

(*) Sequences

signature SEQUENCE =
sig
   type 'a seq
   viewtype 'a cons_seq = 'a seq as Empty | Cons of 'a * 'a seq
   val append : 'a seq * 'a seq -> 'a seq
end

structure ConsList :> SEQUENCE =
struct
   datatype 'a seq = Empty | Cons of 'a * 'a seq
   datatype cons_seq = datatype seq
   fun append(Empty, ys) = ys
     | append(Cons(x,xs), ys) = Cons(x, append(xs,ys))
end

structure JoinList :> SEQUENCE =
struct
   datatype 'a seq = Nil | One of 'a | App of 'a seq * 'a seq
   (* Invariant: Nil never child of App *)
   fun append(xs, Nil) = xs
     | append(Nil, ys) = ys
     | append(xs,  ys) = App(xs,ys)
   viewtype 'a cons_seq = 'a seq as Empty | Cons of 'a * 'a seq
   with
      fun to Nil            = Empty
        | to(One x)         = Cons(x, Nil)
        | to(App(xs,ys))    = let val Cons(x,xs') = to xs
			      in Cons(x, append(xs', ys)) end
      fun from Empty        = Nil
        | from(Cons(x,xs))  = append(One x, xs)
   end
end

functor Map(include SEQUENCE) =
struct
   fun map f Empty = Empty
     | map f (Cons(x,xs)) = Cons(f x, map f xs)
end

structure ConsMap = Map ConsList
structure JoinMap = Map JoinList

signature SEQUENCE =
sig
   type 'a seq
   viewtype 'a cons_seq = 'a seq as EmptyC | Cons of 'a * 'a seq
   viewtype 'a snoc_seq = 'a seq as EmptyS | Snoc of 'a seq * 'a
end


(*) Miscellaneous

viewtype ''a same_diff = ''a * ''a as Same of ''a | Diff of ''a * ''a
with
   fun to(x,y)       = if x = y then Same x else Diff(x,y)
   fun from(Same x)  = (x,x)
     | from(Diff xy) = xy
end

viewtype minmax = int * int as MinMax of int * int
with
   fun to(x,y)         = if x < y then MinMax(x,y) else MinMax(y,x)
   fun from(MinMax xy) = xy
end

viewtype 'a last = 'a list as Last of 'a | NoLast
with
   fun to nil       = NoLast
     | to [x]       = Last x
     | to(x::xs)    = to xs
   fun from NoLast  = nil
     | from(Last x) = [x]
end


(*) Example by Vesa Karvonnen
(*) Posted on FreeNode #sml

(*) Infix product constructor

infix &
viewtype ('a, 'b) product = 'a * 'b as & of 'a * 'b
with
  val to = op&
  fun from (op & x) = x
end

val quadruple = 1 & "2" & true & 0.1
val (a,b,c,d) = quadruple
val a & b & c & d = quadruple
val x & y = quadruple
