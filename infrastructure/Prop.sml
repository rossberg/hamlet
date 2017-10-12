(*
 * (c) Andreas Rossberg 2013
 *
 * Property lists
 *)

structure Prop :> PROP =
struct
  type nil           = unit
  type ('a, 'b) cons = {hd : 'a option ref, tl : 'b option ref}
  type 'a prop       = 'a option ref

  val none = ()
  fun new() = {hd = ref NONE, tl = ref NONE}
  fun hd{hd, tl} = hd
  fun tl{hd, tl = tl as ref NONE}    = let val A = new() in tl := SOME A; A end
    | tl{hd, tl = tl as ref(SOME A)} = A

  fun has p = Option.isSome(!p)
  fun try p = !p
  fun get p = Option.valOf(!p)
  fun set(p, x) = p := SOME x
end;
