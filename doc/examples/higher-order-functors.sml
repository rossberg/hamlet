(*****************************************************************************)
(*) Example from:
(*) Mads Tofte
(*) Principal Signatures for Higher-Order Program Modules
(*) 19th Symposium on Principles of Programming Languages, 1992
(*****************************************************************************)

(*) First order

signature MONOID =
sig
   type t
   val e : t
   val plus : t * t -> t
end

functor Prod (M : MONOID) (N : MONOID) =
struct
   type t = M.t * N.t
   val e = (M.e, N.e)
   fun plus((x1,y1), (x2,y2)) = (M.plus(x1,x2), N.plus(y1,y2))
end

functor Square (M : MONOID) : MONOID = Prod M M

structure Plane = Square (type t = real val e = 0.0 val plus = Real.+)
val x = Plane.plus(Plane.e, (7.4,5.4))


(*) Higher order

signature PROD = MONOID -> MONOID -> MONOID

functor Square (M : MONOID) (Prod : PROD) : MONOID = Prod M M

structure T = Square Plane Prod
val x = T.plus(T.e, T.e)


(*) Transparently

signature PROD' =
   fct M : MONOID -> fct N : MONOID -> MONOID where type t = M.t * N.t

functor Square' (M : MONOID) (Prod : PROD') : MONOID = Prod M M

structure T' = Square' Plane Prod
val x = T'.plus(T'.e, ((7.4,5.4), (3.0,1.7)))


(*****************************************************************************)
(*) Example from:
(*) David MacQueen, Mads Tofte
(*) A Semantics for Higher-Order Functors
(*) European Symposium on Programming, 1994
(*****************************************************************************)

signature POINT =
sig
   type point
   val compare : point * point -> order
end

signature INTERVAL =
sig
   type point
   type interval
   val interval : point * point -> interval
   val left : interval -> point
   val right : interval -> point
end

functor MkInterval(P : POINT) : INTERVAL =
struct
   type point = P.point
   type interval = point * point
   fun interval(x,y) = if P.compare(x,y) = LESS then (x,y) else (y,x)
   fun left(x,_) = x
   fun right(_,y) = y
end

structure IntPoint : POINT =
struct
   type point = int
   val compare = Int.compare
end

structure IntInterval = MkInterval IntPoint
val x = let open IntInterval in right(interval(4,3)) + 5 end


functor G (MkInterval : fct P : POINT -> INTERVAL where type point = P.point)
	= MkInterval IntPoint
structure IntInterval = G MkInterval
val x = let open IntInterval in right(interval(4,3)) + 5 end

(*) Can *not* express the following, which is addressed by the more complex
(*) functor semantics in the paper:
(*
functor G (MkInterval : POINT -> INTERVAL)
	= MkInterval IntPoint
structure IntInterval = G MkInterval
val x = let open IntInterval in right(interval(4,3)) + 5 end (*) ill-typed!
*)


(*****************************************************************************)
(*) Examples from:
(*) Xavier Leroy
(*) Applicative functors and fully transparent higher-order modules
(*) 22nd Symposium on Principles of Programming Languages, 1995
(*****************************************************************************)

signature S = sig type t end

functor Apply (F : S -> S) (X : S) = F X
functor Apply' (F : fct X : S -> S where type t = X.t) (X : S) = F X
functor Apply'' (X : S) (F : (S where type t = X.t) -> S) = F X

structure Apply = Apply : (S -> S) -> S -> S
structure Apply' = Apply' : (fct X : S -> S where type t = X.t) -> S -> S
structure Apply'' = Apply'' : fct X : S -> ((S where type t = X.t) -> S) -> S

structure A  = struct type t = int end
structure B  = Apply (fct X : S => X) A
structure B' = Apply (fct X : S => struct type t = X.t end) A
structure B''= Apply (fct (type t) => (type t = t)) A
structure C  = Apply (fct X : S => struct type t = bool end) A
structure D  = Apply' (fct X : S => X) A
structure D' = Apply' (fct X : S => struct type t = X.t end) A
structure E  = Apply'' A (fct X : S where type t = A.t => X)
structure E' = Apply'' A (fct (type t = A.t) => (type t = t))
structure F  = Apply'' A (fct X : (type t = A.t) => struct type t = bool end)
structure F' = Apply'' A (fct (type t = A.t) => (type t = bool))

functor ApplyProd (F : S -> S) (type t) = F (type t = t * t)

structure ApplyProd = ApplyProd : (S -> S) -> S -> S


(*****************************************************************************)
(*) Examples from:
(*) Sandip Biswas
(*) Higher-order functors with transparent signatures
(*) 22nd Symposium on Principles of Programming Languages, 1995
(*****************************************************************************)

signature ORDERED =
sig
   type t
   val compare : t * t -> order
end

signature SET =
sig
   type set
   type elem
   val empty : set
   val insert : set * elem -> set
   val elem : elem * set -> bool
   val size : set -> int
end

functor ListSet(Elem : ORDERED) :> (SET where type elem = Elem.t) =
struct
   type elem = Elem.t
   type set  = elem list
   val empty = nil
   fun insert(nil, x) = [x]
     | insert(y::ys as s, x) =
       case Elem.compare(x,y) of
       | LESS => x::s
       | EQUAL => s
       | GREATER => y::insert(ys, x)
   fun elem(x, nil) = false
     | elem(x, y::ys) =
       case Elem.compare(x,y) of
       | LESS => false
       | EQUAL => true
       | GREATER => elem(x, ys)
   val size = List.length
end

functor TreeSet(Elem : ORDERED) :> (SET where type elem = Elem.t) =
struct
   type elem = Elem.t
   datatype set = Empty | Elem of set * elem * set
   val empty = Empty
   fun insert(Empty, x) = Elem(Empty, x, Empty)
     | insert(Elem(s1,y,s2) as s, x) =
       case Elem.compare(x,y) of
       | LESS => Elem(insert(s1,x), y, s2)
       | EQUAL => s
       | GREATER => Elem(s1, y, insert(s2, x))
   fun elem(x, Empty) = false
     | elem(x, Elem(s1,y,s2)) =
       case Elem.compare(x,y) of
       | LESS => elem(x, s1)
       | EQUAL => true
       | GREATER => elem(x, s2)
   fun size Empty = 0
     | size(Elem(s1,_,s2)) = size s1 + 1 + size s2
end

functor ExtendSet (MkSet : fct Elem : ORDERED -> SET where type elem = Elem.t)
                  (Elem : ORDERED) =
struct
   open (MkSet Elem)
   fun closure f s =
       let
	  val s' = f s
       in
	  if size s' = size s then s else closure f s'
       end
end

structure Set1 = ExtendSet ListSet (type t = int open Int)
structure Set2 = ExtendSet TreeSet (type t = int open Int)


(*****************************************************************************)
(*) Examples from:
(*) Claudio Russo
(*) Types for Modules
(*) Doctoral Dissertation, University of Edinburgh, 1998
(*****************************************************************************)

(*) Signatures for natural numbers

signature NAT =
sig
   type nat
   val zero : nat
   val succ : nat -> nat
   val iter : 'a -> ('a -> 'a) -> nat -> 'a
end

signature ADD =
sig
   type nat
   val add : nat -> nat -> nat
end

signature MUL =
sig
   type nat
   val mul : nat -> nat -> nat
end


(*) A functor for representing polynomials

functor MkPoly (Nat : NAT)
               (MkAdd : (NAT where type nat = Nat.nat) ->
			(ADD where type nat = Nat.nat))
               (MkMul : (NAT where type nat = Nat.nat) ->
			((NAT where type nat = Nat.nat) -> 
			 (ADD where type nat = Nat.nat)) ->
			(MUL where type nat = Nat.nat)) =
struct
   structure Nat = Nat
   structure Add = MkAdd Nat
   structure Mul = MkMul Nat MkAdd
   fun eval x nil     = Nat.zero
     | eval x (t::ts) = Add.add t (Mul.mul x (eval x ts))
end


(*) A prototype implementation of naturals as peano numbers

structure PeanoNat :> NAT =
struct
   datatype nat = zero | succ of nat
   fun iter z s zero     = z
     | iter z s (succ n) = s(iter z s n)
end

functor MkAdd(Nat : NAT) : ADD =
struct
   open Nat
   fun add n = iter n succ
end

functor MkMul (Nat : NAT)
	      (MkAdd : (NAT where type nat = Nat.nat) ->
		       (ADD where type nat = Nat.nat)) : MUL =
struct
   open Nat
   open (MkAdd Nat)
   fun mul n = iter zero (add n)
end

structure PeanoPoly  = MkPoly PeanoNat MkAdd MkMul


(*) Composition is still possible with an enriched addition functor

functor MkAdd'(Nat : NAT) : ADD =
struct
   open Nat
   fun add n = iter n succ
   fun sum nil = zero
     | sum(n::ns) = add n (sum ns)
end

structure PeanoPoly' = MkPoly PeanoNat MkAdd' MkMul


(*) Efficient implementation using integers

structure FastNat : NAT =
struct
   type nat = int
   val zero = 0
   fun succ n = n+1
   fun iter z s 0 = z
     | iter z s n = s(iter z s (n-1))
end

functor MkFastAdd () : ADD =
struct
   open FastNat
   fun add m n = m + n
end

functor MkFastMul () (MkAdd : (NAT where type nat = int) -> ()) : MUL =
struct
   open FastNat
   fun mul m n = m * n
end

structure FastPoly = MkPoly FastNat MkFastAdd MkFastMul


(*) Or with proper sealing

structure Fast =
struct
   structure Nat = FastNat
   structure MkAdd = MkFastAdd
   structure MkMul = MkFastMul
end :>
sig
   structure Nat : NAT
   functor MkAdd () : ADD where type nat = Nat.nat
   functor MkMul () (MkAdd : (NAT where type nat = Nat.nat) -> ())
	   : MUL where type nat = Nat.nat
end

structure FastPoly = MkPoly Fast.Nat Fast.MkAdd Fast.MkMul


(*****************************************************************************)
(*) Example from:
(*) Andreas Rossberg
(*) Typed Open Programming
(*) PhD Thesis, Universität des Saarlandes, 2007
(*****************************************************************************)

signature COMPILER_STAGE =
sig
   type in_rep
   type out_rep
   type context
   val run : context -> in_rep -> out_rep
end

functor ComposeStages
   (Stage1 : COMPILER_STAGE)
   (Stage2 : COMPILER_STAGE where type in_rep = Stage1.out_rep)
   : COMPILER_STAGE =
struct
   type in_rep = Stage1.in_rep
   type out_rep = Stage2.out_rep
   type context = Stage1.context * Stage2.context
   fun run (c1,c2) = Stage2.run c2 o Stage1.run c1
end


signature CONFIG =
sig
   type logstream
   exception Error
   val errLog : logstream
   val warnLog : logstream
   val log : logstream * string -> unit
end

functor ComposeConfiguredStages
   (type inter_rep)
   (MkStage1 : CONFIG -> COMPILER_STAGE where type out_rep = inter_rep)
   (MkStage2 : CONFIG -> COMPILER_STAGE where type in_rep = inter_rep)
   (Config : CONFIG)
   : COMPILER_STAGE =
let
   structure Stage1 = MkStage1 Config
   structure Stage2 = MkStage2 Config
in
   ComposeStages Stage1 Stage2
end
