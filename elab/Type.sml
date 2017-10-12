(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML types
 *
 * Definition, Section 4.2 and 4.4
 *
 * Notes: see TYPE-sig.sml
 *)

structure Type :> TYPE =
struct
  (* Import *)

  open StaticObjectsCore

  type Lab          = Lab.Lab
  type 'a TyVarMap  = 'a TyVarMap.map
  type 'a TyNameMap = 'a TyNameMap.map


  (* Types [Section 4.2 and 5.2] *)

  type Substitution = Type TyVarMap                     (* [mu] *)
  type Realisation  = TypeFcn TyNameMap                 (* [phi] *)


  (* Creation *)

  fun fromTyVar alpha        = ref(TyVar alpha)
  fun fromRowType rho        = ref(RowType rho)
  fun fromFunType x          = ref(FunType x)
  fun fromConsType x         = ref(ConsType x)
  fun fromOverloadingClass O = ref(Overloaded O)

  fun invent eq = ref(ConsType([], TyName.invent(0, eq)))

  fun guess eq =
      let
        val stamp = Stamp.stamp()
      in
        ref(Undetermined{stamp = stamp, eq = eq, time = stamp})
      end


  (* Projections *)

  exception Type

  fun determined(ref(Determined tau)) = determined tau
    | determined(ref tau')            = tau'

  fun toTyVar tau =
        case determined tau of TyVar tyvar => tyvar | _ => raise Type
  fun toRowType tau =
        case determined tau of RowType rho => rho | _ => raise Type
  fun toFunType tau =
        case determined tau of FunType x => x | _ => raise Type
  fun toConsType tau =
        case determined tau of ConsType x => x | _ => raise Type

  fun isOverloaded tau =
        case determined tau of Overloaded _ => true | _ => false

  fun tyname(ref(FunType(tau1, tau2))) = tyname tau2
    | tyname(ref(ConsType(taus, t)))   = t
    | tyname(ref(Determined(tau)))     = tyname tau
    | tyname  _                        = raise Type


  (* Determination with an explicit map *)

  fun determine det tau = tau := determine'(det, !tau)
  and determine'(det, tau' as TyVar alpha) =
        tau'
    | determine'(det, tau' as RowType(rho, r)) =
        ( LabMap.app (determine det) rho; tau' )
    | determine'(det, tau' as FunType(tau1, tau2)) =
        ( determine det tau1; determine det tau2; tau' )
    | determine'(det, tau' as ConsType(taus, t)) =
        ( List.app (determine det) taus; tau' )
    | determine'(det, tau' as Undetermined{stamp, ...}) =
        (case StampMap.find(det, stamp) of
          SOME tau => Determined tau
        | NONE     => tau'
        )
    | determine'(det, tau' as Overloaded O) =
        tau'
    | determine'(det, tau' as Determined tau) =
        ( determine det tau; tau' )


  (* Cloning under a substitution and a type realisation *)

  fun clone (mu, phi) tau =
      let
        (* Cloning must respect sharing (at least for not fully
         * determined types). An association list is used to remember
         * nodes already visited together with their copy.
         *)
        val mu'    = ref mu
        val cloned = ref []

        fun clone tau =
            case List.find (fn(tau1, _) => tau1 = tau) (!cloned) of
              SOME(_, tau2) => tau2
            | NONE =>
                let val tau2 = clone' tau in
                  cloned := (tau, tau2) :: !cloned;
                  tau2
                end
        and clone' tau =
            case !tau of
              TyVar(alpha) =>
                (case TyVarMap.find(!mu', alpha) of
                  NONE     => tau
                | SOME tau => tau
                )
            | RowType(rho, NONE) =>
                (* If row is closed, we can safely copy. *)
                ref(RowType(LabMap.map clone rho, NONE))
            | RowType(rho, SOME _) =>
                (* If the row is not closed, than we must keep sharing!
                 * The row may not contain any tynames or tyvars
                 * of the domains of mu and phi in this case.
                 * We conjecture that this does not happen since
                 * the only possibility for this case is instantiation
                 * of a local type scheme, where the type (and thus
                 * all contained tyvars) will be bound in the context.
                 *)
                tau
            | FunType(tau1, tau2) =>
                ref(FunType(clone tau1, clone tau2))
            | ConsType(taus, t) =>
                let
                  fun isTyVar(ref(TyVar alpha'), alpha) = alpha = alpha'
                    | isTyVar(_, alpha)                 = false

                  fun isTyName((alphas, ref(ConsType(taus, t1))), t2) =
                        t1 = t2 andalso ListPair.allEq isTyVar (taus, alphas)
                    | isTyName _ = false

                  val taus2 = List.map clone taus
                in
                  case TyNameMap.find(phi, t) of
                    NONE => ref(ConsType(taus2, t))
                  | SOME theta =>
                      if isTyName(theta, t) then ref(ConsType(taus2, t)) else
                      let
                        val (alphas, tau1) = renameTypeFcn theta
                        val oldCloned      = !cloned
                      in
                        mu' :=
                          ListPair.foldlEq
                            (fn(alpha, tau2, mu) =>
                              TyVarMap.insert(mu, alpha, tau2))
                            (!mu') (alphas, taus2);
                        clone' tau1 before cloned := oldCloned
                      end
                end
            | Undetermined _ =>
                tau
            | Overloaded O =>
                tau
            | Determined tau =>
                clone tau
        in
            clone tau
        end

  and renameTypeFcn(alphas, tau) =
      let
        val alphas' = List.map (TyVar.invent o TyVar.admitsEquality) alphas
        val taus    = List.map fromTyVar alphas'
        val mu      = TyVarMap.fromList(ListPair.zipEq(alphas, taus))
      in
        (alphas', substitute mu tau)
      end


  (* Substitution, realisation [Section 5.2] *)

  and substitute mu = clone(mu, TyNameMap.empty)
  fun realise phi   = clone(TyVarMap.empty, phi)


  (* Type variable and type name extraction [Section 4.2] *)

  fun tyvars(ref tau') = tyvars' tau'
  and tyvars'(TyVar alpha) =
        TyVarSet.singleton alpha
    | tyvars'(RowType(rho, r)) =
        LabMap.foldl (fn(tau, U) => TyVarSet.union(U, tyvars tau))
          TyVarSet.empty rho
    | tyvars'(FunType(tau1, tau2)) =
        TyVarSet.union(tyvars tau1, tyvars tau2)
    | tyvars'(ConsType(taus, t)) =
        List.foldl (fn(tau, U) => TyVarSet.union(U, tyvars tau))
          TyVarSet.empty taus
    | tyvars'(Undetermined _) =
        (* Not quite right, but we never fill in types containing tyvars. *)
        TyVarSet.empty
    | tyvars'(Overloaded O) =
        TyVarSet.empty
    | tyvars'(Determined tau) =
        tyvars tau

  fun tynames(ref tau') = tynames' tau'
  and tynames'(TyVar alpha) =
        TyNameSet.empty
    | tynames'(RowType(rho, r)) =
        LabMap.foldl (fn(tau, T) => TyNameSet.union(T, tynames tau))
          TyNameSet.empty rho
    | tynames'(FunType(tau1, tau2)) =
        TyNameSet.union(tynames tau1, tynames tau2)
    | tynames'(ConsType(taus, t)) =
        List.foldl (fn(tau, T) => TyNameSet.union(T, tynames tau))
          (TyNameSet.singleton t) taus
    | tynames'(Undetermined _) =
        (* Not quite right, but currently it is OK for all uses of
         * of this function in HaMLet. :-P *)
        TyNameSet.empty
    | tynames'(Overloaded O) =
        (* Approximation *)
        OverloadingClass.set O
    | tynames'(Determined tau) =
        tynames tau

  fun undetermined(ref tau') = undetermined' tau'
  and undetermined'(TyVar alpha) =
        StampMap.empty
    | undetermined'(RowType(rho, r)) =
        LabMap.foldl (fn(tau, Z) => StampMap.unionWith #2 (Z, undetermined tau))
          StampMap.empty rho
    | undetermined'(FunType(tau1, tau2)) =
        StampMap.unionWith #2 (undetermined tau1, undetermined tau2)
    | undetermined'(ConsType(taus, t)) =
        List.foldl (fn(tau, Z) => StampMap.unionWith #2 (Z, undetermined tau))
          StampMap.empty taus
    | undetermined'(Undetermined{stamp, eq,...}) =
        StampMap.singleton(stamp, eq)
    | undetermined'(Overloaded O) =
        StampMap.empty
    | undetermined'(Determined tau) =
        undetermined tau


  (* Check for equality type [Section 4.4] *)

  fun admitsEquality(ref tau') = admitsEquality' tau'
  and admitsEquality'(TyVar alpha) =
        TyVar.admitsEquality alpha
    | admitsEquality'(RowType(rho, NONE)) =
        LabMap.all admitsEquality rho
    | admitsEquality'(RowType(rho, SOME{eq, ...})) =
        LabMap.all admitsEquality rho andalso eq orelse raise Type
    | admitsEquality'(FunType _) =
        false
    | admitsEquality'(ConsType(taus, t)) =
        TyName.admitsEquality t andalso List.all admitsEquality taus
        orelse TyName.toString t = "ref"
    | admitsEquality'(Undetermined{eq, ...}) =
        eq orelse raise Type
    | admitsEquality'(Overloaded O) =
        raise Type
    | admitsEquality'(Determined tau) =
        admitsEquality tau


  (* Equality *)

  fun equals(ref(Determined tau1), tau2) = equals(tau1, tau2)
    | equals(tau1, ref(Determined tau2)) = equals(tau1, tau2)
    | equals(tau1 as ref tau1', tau2 as ref tau2') =
        tau1 = tau2 orelse equals'(tau1', tau2')
  and equals'(TyVar alpha1, TyVar alpha2) =
        alpha1 = alpha2
    | equals'(FunType(tau11, tau12), FunType(tau21, tau22)) =
        equals(tau11, tau21) andalso equals(tau12, tau22)
    | equals'(RowType(rho1, r1), RowType(rho2, r2)) =
      let
        fun equalsField(lab, tau1) =
            case LabMap.find(rho2, lab) of
              SOME tau2 => equals(tau1, tau2)
            | NONE      => false
      in
        r1 = r2 andalso LabMap.numItems rho1 = LabMap.numItems rho2
          andalso LabMap.alli equalsField rho1
      end
    | equals'(tau' as ConsType(taus1, t1), ConsType(taus2, t2)) =
        t1 = t2 andalso ListPair.allEq equals (taus1, taus2)
    | equals'(Undetermined{stamp = z1, ...}, Undetermined{stamp = z2, ...}) =
        z1 = z2 orelse raise Type
    | equals'(Overloaded O1, Overloaded O2) =
        raise Type
    | equals' _ =
        false


  (* Unification *)

  exception Unify

  fun occurs z (ref tau') = occurs'(z, tau')
  and occurs'(z, TyVar alpha) =
        false
    | occurs'(z, RowType(rho, r)) =
        LabMap.exists (occurs z) rho
    | occurs'(z, FunType(tau1, tau2)) =
        occurs z tau1 orelse occurs z tau2
    | occurs'(z, ConsType(taus, t)) =
        List.exists (occurs z) taus
    | occurs'(z, Undetermined{stamp, ...}) =
        stamp = z
    | occurs'(z, Overloaded O) =
        false
    | occurs'(z, Determined tau) =
        occurs z tau

  fun unify(ref(Determined tau1), tau2) =
        unify(tau1, tau2)
    | unify(tau1, ref(Determined tau2)) =
        unify(tau1, tau2)
    | unify(tau1 as ref tau1', tau2 as ref tau2') =
        if tau1 = tau2 then ()
        else ( tau1 := unify'(tau1', tau2'); tau2 := Determined tau1 )
  and unify'(Undetermined{stamp, eq, time}, tau') =
        unifyUndetermined(stamp, eq, time, tau')
    | unify'(tau', Undetermined{stamp, eq, time}) =
        unifyUndetermined(stamp, eq, time, tau')
    | unify'(Overloaded O, tau') =
        unifyOverloaded(O, tau')
    | unify'(tau', Overloaded O) =
        unifyOverloaded(O, tau')
    | unify'(tau' as TyVar alpha1, TyVar alpha2) =
        if alpha1 = alpha2 then tau' else raise Unify
    | unify'(tau' as FunType(tau11, tau12), FunType(tau21, tau22)) =
        ( unify(tau11, tau21); unify(tau12, tau22); tau' )
    | unify'(RowType(rho1, r1), RowType(rho2, r2)) =
      let
        fun unifyField r (lab, tau1, rho) =
            case LabMap.find(rho, lab) of
              SOME tau2  => ( unify(tau1, tau2); #1(LabMap.remove(rho, lab)) )
            | NONE =>
                case r of
                  NONE           => raise Unify
                | SOME{eq, time} => ( propagate (time, eq) tau1; rho )

        val rho1' = LabMap.foldli (unifyField r1) rho1 rho2
        val _     = LabMap.foldli (unifyField r2) rho2 rho1'
        val r     =
            case (r1, r2) of
              (NONE, _) => NONE
            | (_, NONE) => NONE
            | (SOME{eq = eq1, time = time1}, SOME{eq = eq2, time = time2}) =>
                SOME{eq = eq1 orelse eq2, time = Stamp.min(time1, time2)}
      in
        RowType(LabMap.unionWith #2 (rho2, rho1'), r)
      end
    | unify'(tau' as ConsType(taus1, t1), ConsType(taus2, t2)) =
        if t1 = t2 then
          ( ListPair.appEq unify (taus1, taus2); tau' )
        else
          raise Unify
    | unify' _ =
        raise Unify

  and unifyUndetermined(z, eq, time, tau') =
        if occurs'(z, tau') then
          raise Unify
        else
          propagate'(time, eq) tau'

  and unifyOverloaded(O, Undetermined{stamp, eq, time}) =
        unifyUndetermined(stamp, eq, time, Overloaded O)
    | unifyOverloaded(O, tau' as ConsType([], t)) =
        if OverloadingClass.member(O, t) then tau' else raise Unify
    | unifyOverloaded(O1, Overloaded(O2)) =
        (case OverloadingClass.intersection(O1, O2) of
          NONE   => raise Unify
        | SOME O => Overloaded O
        )
    | unifyOverloaded(O, _) =
        raise Unify

  and propagate (time, eq) (tau as ref tau') = tau := propagate'(time, eq) tau'
  and propagate'(time, eq) (tau' as TyVar alpha) =
        if not eq orelse TyVar.admitsEquality alpha then tau' else raise Unify
    | propagate'(time, eq) (RowType(rho, r)) =
        ( LabMap.app (propagate(time, eq)) rho;
          RowType(rho, Option.map (propagateRowVar(time, eq)) r)
        )
    | propagate'(time, eq) (tau' as FunType(tau1, tau2)) =
        if eq then raise Unify
        else ( propagate (time, eq) tau1; propagate (time, eq) tau2; tau' )
    | propagate'(time, eq) (tau' as ConsType(taus, t)) =
        (case Stamp.compare(TyName.time t, time) of
          GREATER => raise Unify
        | _ =>
            if not eq orelse TyName.toString t = "ref" then
              ( List.app (propagate(time, false)) taus; tau' )
            else if TyName.admitsEquality t then
              ( List.app (propagate(time, eq)) taus; tau' )
            else
              raise Unify
        )
    | propagate'(time, eq) (Undetermined{stamp = z, eq = eq', time = time'}) =
        Undetermined{
          stamp = z, eq = eq orelse eq', time = Stamp.min(time, time')}
    | propagate'(time, eq) (tau' as Overloaded O) =
        if not eq then tau' else
        (case OverloadingClass.makeEquality O of
          NONE    => raise Unify
        | SOME O' => Overloaded O'
        )
    | propagate'(time, eq) (tau' as Determined tau) =
        ( propagate (time, eq) tau; tau' )

  and propagateRowVar (time, eq) {eq = eq', time = time'} =
        {eq = eq orelse eq', time = Stamp.min(time, time')}


  (* Assign default to overloaded type and check for remaining
   * flexible types [Appendix E and Section 4.11]
   *)

  exception Flexible

  fun resolve(ref(Determined tau)) =
        resolve tau
    | resolve(tau as ref(Overloaded O)) =
        tau := ConsType([], OverloadingClass.default O)
    | resolve(ref(RowType(rho, SOME _))) =
        raise Flexible
    | resolve _ =
        ()


  (* Operations on rows *)

  val emptyRow                      = (LabMap.empty, NONE) : RowType
  fun guessRow()                    = (LabMap.empty,
                                        SOME{eq = false, time = Stamp.stamp()})
  fun insertRow((rho, r), lab, tau) = (LabMap.insert(rho, lab, tau), r)
  fun findLab((rho, r), lab)        = LabMap.find(rho, lab)

  val rowFromList =
      List.foldl (fn((lab, tau), row) => insertRow(row, lab, tau)) emptyRow

  fun fromTupleType taus =
      fromRowType(rowFromList(ListPair.zip(List.tabulate(
        List.length taus, fn i => Lab.fromInt(i + 1)), taus)))
end;
