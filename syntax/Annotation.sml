(*
 * (c) Andreas Rossberg 2013
 *
 * Helpers for abstract syntax annotations
 *)

structure Annotation : ANNOTATION =
struct
  open Prop

  type 'a annotation = (Source.loc, 'a) cons                   (* [A] *)
  datatype ('a, 'b) phrase = @@ of 'a * 'b annotation

  infix @@
  fun syntax(s@@A) = s
  fun annotation(s@@A) = A

  fun loc A = get(hd A)
  fun fromLoc l = let val A = new() in set(hd A, l); A end
  fun copy A = fromLoc(loc A)
  fun nowhere() = fromLoc Source.nowhere

  fun transform1 f (_@@A) = fromLoc(f(loc A))
  fun transform2 f (_@@A1, _@@A2) = fromLoc(f(loc A1, loc A2))

  fun at(phrase) = transform1 Source.at phrase
  fun left(phrase) = transform1 Source.left phrase
  fun right(phrase) = transform1 Source.right phrase
  fun over(phrase1, phrase2) = transform2 Source.over (phrase1, phrase2)

  fun overSome(phrase1, NONE) = at(phrase1)
    | overSome(phrase1, SOME phrase2) = over(phrase1, phrase2)

  fun overAll[] = raise Empty
    | overAll[phrase] = at(phrase)
    | overAll((phrase as x@@_)::phrases) = over(phrase, x@@overAll(phrases))

  exception Annotation

  infix -->
  fun a --> p = (if has p then raise Annotation else set(p, a); a)
end;
