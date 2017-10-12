(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML exception names and sets thereof
 *
 * Definition, Sections 6.2 and 6.3
 *)


structure ExName :> EXNAME =
struct
  (* Import *)

  type VId   = VId.Id
  type stamp = Stamp.stamp


  (* Type [Section 6.2] *)

  type ExName = {vid : VId, stamp : stamp}                      (* [en] *)


  (* Creation *)

  fun exname vid = {vid = vid, stamp = Stamp.stamp()}


  (* Conversion *)

  fun toString{vid, stamp} = VId.toString vid


  (* Ordering *)

  fun compare(en1 : ExName, en2 : ExName) =
      Stamp.compare(#stamp en1, #stamp en2)
end

structure ExNameSet =
    FinSetFn(type ord_key = ExName.ExName; val compare = ExName.compare);
