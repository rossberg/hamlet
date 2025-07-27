(* fifo-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Applicative fifos
 *
 *)

signature FIFO =
  sig
    type 'a fifo

    exception Dequeue

    val empty : 'a fifo
    val isEmpty : 'a fifo -> bool
    val enqueue : 'a fifo * 'a -> 'a fifo
    val dequeue : 'a fifo -> 'a fifo * 'a
    val delete : ('a fifo * ('a -> bool)) -> 'a fifo
    val head : 'a fifo -> 'a
    val peek : 'a fifo -> 'a option
    val length : 'a fifo -> int
    val contents : 'a fifo -> 'a list
    val app : ('a -> unit) -> 'a fifo -> unit
    val map : ('a -> 'b) -> 'a fifo -> 'b fifo
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a fifo -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a fifo -> 'b

  end (* FIFO *)
