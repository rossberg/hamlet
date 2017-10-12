(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure List :> LIST =
struct
  datatype list               = datatype list

  exception Empty             = Empty

  fun null nil                = true
    | null _                  = false

  fun length nil              = 0
    | length(x::l')           = 1 + length l'

  fun hd(x::l')               = x
    | hd  _                   = raise Empty

  fun tl(x::l')               = l'
    | tl  _                   = raise Empty

  fun last(x::nil)            = x
    | last(x::l')             = last l'
    | last  nil               = raise Empty

  fun getItem(x::l')          = SOME(x, l')
    | getItem nil             = NONE

  fun nth(x::l', 0)           = x
    | nth(x::l', i)           = nth(l', i - 1)
    | nth(nil,   i)           = raise Subscript

  fun rev l                   = rev'(l, nil)
  and rev'(nil, xs)           = xs
    | rev'(x::l', xs)         = rev'(l', x::xs)

  fun   nil    @ l2           = l2
    | (x::l1') @ l2           = x :: l1' @ l2

  fun revAppend( nil,   l2)   = l2
    | revAppend(x::l1', l2)   = revAppend(l1', x::l2)

  fun concat nil              = nil
    | concat(xs::l')          = xs @ concat l'

  fun take(l,     0)          = nil
    | take(x::l', i)          = x :: take(l', i - 1)
    | take(nil,   i)          = raise Subscript

  fun drop(l,     0)          = l
    | drop(x::l', i)          = drop(l', i - 1)
    | drop(nil,   i)          = raise Subscript

  fun app f nil               = ()
    | app f (x::l')           = (f x ; app f l')

  fun map f nil               = nil
    | map f (x::l')           = f x :: map f l'

  fun mapPartial f nil        = nil
    | mapPartial f (x::l')    = case f x of
                                  NONE   => mapPartial f l'
                                | SOME y => y :: mapPartial f l'

  fun find f nil              = NONE
    | find f (x::l')          = if f x then SOME x else find f l'

  fun filter f nil            = nil
    | filter f (x::l')        = if f x then x :: filter f l' else filter f l'

  fun partition f nil         = (nil, nil)
    | partition f (x::l')     = let val (l1, l2) = partition f l'
                                in if f x then (x::l1, l2) else (l1, x::l2) end

  fun foldl f b nil           = b
    | foldl f b (x::l')       = foldl f (f(x, b)) l'

  fun foldr f b nil           = b
    | foldr f b (x::l')       = f(x, foldr f b l')

  fun exists f nil            = false
    | exists f (x::l')        = f x orelse exists f l'
  fun all f l                 = Bool.not(exists (Bool.not o f) l)

  fun tabulate(n, f)          = if n < 0 then raise Size
                                else tabulate'(n, f, 0, nil)
  and tabulate'(n, f, i, l)   = if i = n then rev l
                                else tabulate'(n, f, i + 1, (f i)::l)

  fun collate f (nil, nil)    = EQUAL
    | collate f (nil, l2)     = LESS
    | collate f (l1,  nil)    = GREATER
    | collate f (x1::l1', x2::l2') =
        case f(x1, x2) of
          EQUAL => collate f (l1', l2')
        | other => other
end;
