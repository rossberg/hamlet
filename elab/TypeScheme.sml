(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML type schemes
 *
 * Definition, Section 4.2, 4.5, and 4.8
 *
 * Note: see TYPESCHEME-sig.sml
 *)

structure TypeScheme :> TYPESCHEME =
struct
  (* Import *)

  open StaticObjectsCore

  type Substitution = Type.Substitution
  type Realisation  = Type.Realisation
  type 'a TyNameMap = 'a TyNameMap.map


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


  (* Instantiation *)

  fun instanceTyVar alpha =
        case TyVar.overloadingClass alpha of
          SOME O => Type.fromOverloadingClass O
        | NONE   => Type.guess(TyVar.admitsEquality alpha)

  fun instance(alphas, tau) =
      let
        val taus = List.map instanceTyVar alphas
        val mu   = TyVarMap.fromList(ListPair.zipEq(alphas, taus))
      in
        (taus, Type.substitute mu tau)
      end


  (* Skolemisation *)

  fun skolemiseTyVar alpha =
        case TyVar.overloadingClass alpha of
          NONE   => Type.invent(TyVar.admitsEquality alpha)
        | SOME O => Type.fromConsType([], OverloadingClass.default O)

  fun skolemise(alphas, tau) =
      let
        val taus = List.map skolemiseTyVar alphas
        val mu   = TyVarMap.fromList(ListPair.zipEq(alphas, taus))
      in
        (taus, Type.substitute mu tau)
      end


  (* Generalisation [Section 4.5] *)

  fun generalisesType(sigma, tau) =
        ( Type.unify(#2(instance sigma), tau); true )
        handle Type.Unify => false

  fun generalises(sigma1, sigma2) =
        generalisesType(sigma1, #2(skolemise sigma2))


  (* Comparison [Section 4.5] *)

  fun equals(sigma1, sigma2) =
        generalises(sigma1, sigma2) andalso generalises(sigma2, sigma1)


  (* Closure [Section 4.8] *)

  (* Does not copy! *)
  fun Clos tau = (TyVarSet.listItems(Type.tyvars tau), tau)

  fun ClosRestricted U tau =
        (TyVarSet.listItems(TyVarSet.difference(Type.tyvars tau, U)), tau)

  fun isClosed(alphas, tau) =
        TyVarSet.isSubset(Type.tyvars tau, TyVarSet.fromList alphas)


  (* Normalisation (for output) *)

  fun normaliseTyVar(alpha, i) =
      let
        val alpha' = TyVar.fromInt (TyVar.admitsEquality alpha) i
      in
        case TyVar.overloadingClass alpha of
          NONE   => alpha'
        | SOME O => TyVar.fromOverloadingClass(TyVar.toString alpha', O)
      end

  fun normalise(alphas, tau) =
      let
        val ns      = List.tabulate(List.length alphas, fn i => i) 
        val alphas' = ListPair.mapEq normaliseTyVar (alphas, ns)
        val taus    = List.map Type.fromTyVar alphas'
        val mu      = TyVarMap.fromList(ListPair.zipEq(alphas, taus))
      in
        (alphas', Type.substitute mu tau)
      end


  (* Realisation [Section 5.2] and substitution *)

  fun realise phi (alphas, tau) = (alphas, Type.realise phi tau)

  fun substitute mu (alphas, tau) =
      let
        val mu' =
            List.foldl (fn(alpha, mu) => TyVarMap.delete(mu, alpha)) mu alphas
      in
        (alphas, Type.substitute mu' tau)
      end
end;
