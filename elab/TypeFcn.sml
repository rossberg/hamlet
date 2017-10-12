(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML type functions
 *
 * Definition, Section 4.2, 4.4, and 4.8
 *
 * Notes: see TYPEFCN-sig.sml
 *)

structure TypeFcn :> TYPEFCN =
struct
  (* Import *)

  open StaticObjectsCore

  type Realisation = Type.Realisation


  (* Renaming *)

  fun rename(alphas, tau) =
      let
        val alphas' = List.map (TyVar.invent o TyVar.admitsEquality) alphas
        val taus    = List.map Type.fromTyVar alphas'
        val mu      = TyVarMap.fromList(ListPair.zipEq(alphas, taus))
      in
        (alphas', Type.substitute mu tau)
      end


  (* Type variable and type name extraction [Section 4.2] *)

  fun tyvars(alphas, tau) =
      let
        val U = Type.tyvars tau
      in
        List.foldl
          (fn(alpha, U) =>
            TyVarSet.delete(U, alpha) handle TyVarSet.NotFound => U
          ) U alphas
      end

  fun tynames(alphas, tau)      = Type.tynames tau
  fun undetermined(alphas, tau) = Type.undetermined tau


  (* Arity [Section 4.4] *)

  fun arity(alphas, tau) = List.length alphas


  (* Comparison [Section 4.5] *)

  fun equals((alphas1, tau1), (alphas2, tau2)) =
        List.length alphas1 = List.length alphas2 andalso
        let
          val taus2 = List.map Type.fromTyVar alphas2
          val mu    = TyVarMap.fromList(ListPair.zipEq(alphas1, taus2))
        in
          Type.equals(Type.substitute mu tau1, tau2)
        end


  (* Equality [Section 4.4] *)

  fun admitsEquality(alphas, tau) =
      let
        val taus = List.map (fn _ => Type.guess true) alphas
        val mu   = TyVarMap.fromList(ListPair.zipEq(alphas, taus))
      in
        Type.admitsEquality(Type.substitute mu tau)
      end
    

  (* Eta-conversion [Section 4.4] *)

  fun fromTyName t =
      let
        val alphas = List.tabulate(TyName.arity t, TyVar.fromInt false)
      in
        (alphas, Type.fromConsType(List.map Type.fromTyVar alphas, t))
      end

  fun toTyNameOpt(alphas, ref(ConsType(taus, t))) =
      let
        fun isSame(alpha, ref(TyVar alpha')) = alpha = alpha'
          | isSame(alpha,          _       ) = false
      in
        if ListPair.allEq isSame (alphas, taus) then
          SOME t
        else
          NONE
      end
    | toTyNameOpt _ = NONE

  fun isTyName theta = Option.isSome(toTyNameOpt theta)
  fun toTyName theta =
      case toTyNameOpt theta of
        SOME t => t
      | NONE => raise Type.Type


  (* Application [Section 4.4] *)

  exception Apply

  fun apply(taus, (alphas, tau)) =
      let
        val mu = TyVarMap.fromList(ListPair.zipEq(alphas, taus))
            handle ListPair.UnequalLengths => raise Apply
      in
        Type.substitute mu tau
      end


  (* Normalisation (for output) *)

  fun normalise(alphas, tau) =
      let
        val ns = List.tabulate(List.length alphas, fn n => n) 
        val alphas' =
            ListPair.mapEq
              (fn(alpha, n) => TyVar.fromInt (TyVar.admitsEquality alpha) n)
              (alphas, ns)
        val taus = List.map Type.fromTyVar alphas'
        val mu   = TyVarMap.fromList(ListPair.zipEq(alphas, taus))
      in
        (alphas', Type.substitute mu tau)
      end


  (* Realisation [Section 5.2] *)

  fun realise phi (alphas, tau) = (alphas, Type.realise phi tau)
end;
