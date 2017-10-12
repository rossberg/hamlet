(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML overloading classes
 *
 * Definition, Appendix E
 *
 * Notes: see OVERLOADINGCLASS-sig.sml
 *)

structure OverloadingClass :> OVERLOADINGCLASS =
struct
  (* Import types *)

  type TyName    = TyName.TyName
  type TyNameSet = TyNameSet.set


  (* Type *)

  type OverloadingClass = TyNameSet * TyName            (* [O] *)


  (* Simple operations *)

  fun make O             = O
  fun isEmpty(T, t)      = TyNameSet.isEmpty T
  fun set(T, t)          = T
  fun default (T, t)     = t
  fun member((T, t), t') = TyNameSet.member(T, t')


  (* Modification and combination *)

  exception OverloadingClass

  fun makeEquality (T, t) =
      let
        val T' = TyNameSet.filter TyName.admitsEquality T
      in
        if TyNameSet.isEmpty T' then
          NONE
        else if TyName.admitsEquality t then
          SOME(T', t)
        else
          raise OverloadingClass
      end

  fun union((T1, t1), (T2, t2)) = (TyNameSet.union(T1, T2), t2)

  fun intersection((T1, t1), (T2, t2)) =
      let
        val T' = TyNameSet.intersection(T1, T2)
      in
        if TyNameSet.isEmpty T' then
          NONE
        else if t1 = t2 then
          SOME(T', t1)
        else
          case (TyNameSet.member(T', t1), TyNameSet.member(T', t2)) of
            (true, false) => SOME(T', t1)
          | (false, true) => SOME(T', t2)
          | _             => raise OverloadingClass
      end
end;
