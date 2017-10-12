(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML types
 *
 * Definition, Section 4.2 and 4.4
 * + RFC: Record extension
 * + RFC: First-class modules
 *
 * Notes:
 *   - Types are references so that unification can work via side effects.
 *     We need links (forwards) to unify two type variables.
 *   - Types built bottom-up have to be `normalised' to induce the required
 *     sharing on type variables.
 *   - Care has to be taken to clone types at the proper places.
 *   - Undetermined types are represented separately from type variables.
 *     They carry an additional time stamp to detect attempts of referring
 *     types not in scope. The time stamps are also used to prevent invalid
 *     unification with skolem types (i.e. newer type names) during signature
 *     matching. Time stamps are propagated during unification.
 *   - Substitution creates a clone, but shares undetermined types.
 *   - To represent overloaded type (variables), we add a special type.
 *   - Record types may contain a row variable to represent open record types
 *     (which appear during type inference). Flexible rows have to carry an
 *     equality flag and time stamp to properly propagate information enforced
 *     by unification when extending a row.
 *)

structure Type :> TYPE =
struct
    (* Import *)

    open StaticObjectsCore

    type Lab = Lab.Lab

    type 'a TyVarMap      = 'a TyVarMap.map
    type 'a TyNameMap     = 'a TyNameMap.map


    (* Types [Section 4.2 and 5.2] *)

    type Substitution = Type TyVarMap			(* [mu] *)
    type Realisation  = TypeFcn TyNameMap		(* [phi] *)


    (* Recursive import *)

    structure Sig =
    struct
	val tyvars : (Sig' -> TyVarSet) ref =
	    ref (fn _ => raise Fail "Type.Sig.tyvars")
	val tynames : (Sig' -> TyNameSet) ref =
	    ref (fn _ => raise Fail "Type.Sig.tynames")
	val undetermined : (Sig' -> bool StampMap.map) ref =
	    ref (fn _ => raise Fail "Type.Sig.undetermined")
	val realise : (Realisation -> Sig' -> Sig') ref =
	    ref (fn _ => raise Fail "Type.Sig.realise")
	val matches : (Sig' * Sig' -> bool) ref =
	    ref (fn _ => raise Fail "Type.Sig.matches")
    end


    (* Creation *)

    fun fromTyVar alpha        = ref(TyVar alpha)
    fun fromRowType rho        = ref(RowType rho)
    fun fromFunType x          = ref(FunType x)
    fun fromConsType x         = ref(ConsType x)
    fun fromPackType Sigma     = ref(PackType Sigma)
    fun fromOverloadingClass O = ref(Overloaded O)

    fun guess eq               = let val stamp = Stamp.stamp()
				 in ref(Undetermined{stamp = stamp, eq = eq,
						     time = stamp}) end
    fun invent eq              = ref(ConsType([], TyName.invent(0, eq)))

    fun isOverloaded(ref(Determined(tau))) = isOverloaded tau
      | isOverloaded(ref(Overloaded _))    = true
      | isOverloaded _                     = false


    (* Projections *)

    fun range(ref(FunType(tau1,tau2))) = tau2
      | range(ref(Determined(tau)))    = range tau
      | range tau                      = tau

    fun tyname(ref(ConsType(taus,t)))  = t
      | tyname(ref(Determined(tau)))   = tyname tau
      | tyname  _                      =
	    raise Fail "Type.tyname: non-constructed type"


    (* Operations on rows *)

    (* [RFC: Record extension] *)
    val emptyRow              = ref(FixedRow(LabMap.empty))
    fun singletonRow(lab,tau) = ref(FixedRow(LabMap.singleton(lab,tau)))
    fun guessRow()            = ref(FreeRow{eq=false, time=Stamp.stamp(),
					    excluded=LabSet.empty})

    fun insertRow(rho, lab, tau) =
	case !rho
	  of FixedRow fixed =>
		 ref(FixedRow(LabMap.insert(fixed, lab, tau)))
	   | FlexRow{fixed, flex} =>
		 ( excludeRow(flex, lab)
		 ; ref(FlexRow{fixed=LabMap.insert(fixed, lab,tau), flex=flex})
		 )
	   | FreeRow _ =>
		 ( excludeRow(rho, lab)
		 ; ref(FlexRow{fixed=LabMap.singleton(lab,tau), flex=rho})
		 )

    and excludeRow(rho, lab) =
	case !rho
	  of FixedRow fixed =>
	     if LabMap.inDomain(fixed, lab) then
		 raise Fail "Type.excludeRow: existing label"
	     else ()
	   | FlexRow{fixed, flex} =>
	     if LabMap.inDomain(fixed, lab) then
		 raise Fail "Type.excludeRow: existing label"
	     else
		 excludeRow(flex, lab)
	   | FreeRow{eq, time, excluded} =>
		 rho := FreeRow{eq=eq, time=time,
				excluded=LabSet.add(excluded,lab)}

    fun normalizeRow rho =
	case !rho
	  of FixedRow _ => ()
	   | FreeRow _  => ()
	   | FlexRow{fixed, flex} =>
	     (normalizeRow flex;
	      case !flex
	        of FreeRow _ => ()
		 | FixedRow fixed' =>
		      rho := FixedRow(LabMap.unionWith #2 (fixed,fixed'))
		 | FlexRow{fixed=fixed', flex=flex'} =>
		      rho := FlexRow{fixed = LabMap.unionWith #2 (fixed,fixed'),
				     flex = flex'}
	     )

    fun findLab(rho, lab) = (normalizeRow rho; findLab'(!rho, lab))
    and findLab'(FixedRow fixed, lab)      = LabMap.find(fixed, lab)
      | findLab'(FlexRow{fixed, ...}, lab) = LabMap.find(fixed, lab)
      | findLab'(FreeRow _, lab)           = NONE


    (* Cloning under a substitution and a type realisation *)

    fun clone (mu,phi,det) tau =
	let
	    (* Cloning must respect sharing (at least for not fully
	     * determined types). An association list is used to remember
	     * nodes already visited together with their copy.
	     *)

	    val mu'    = ref mu
	    val cloned = ref []
	    val clonedRow = ref []

	    fun clone tau =
		case List.find (fn(tau1,_) => tau1 = tau) (!cloned)
		  of SOME(_,tau2) => tau2
		   | NONE         => let val tau2 = clone' tau in
					 cloned := (tau,tau2) :: !cloned ;
					 tau2
				     end

	    and cloneRow rho =
		case List.find (fn(rho1,_) => rho1 = rho) (!clonedRow)
		  of SOME(_,rho2) => rho2
		   | NONE         => let val rho2 = cloneRow' rho in
					 clonedRow := (rho,rho2) :: !clonedRow ;
					 rho2
				     end

	    and clone' tau =
		case !tau
		  of TyVar(alpha) =>
		     (case TyVarMap.find(!mu', alpha)
			of NONE     => tau
			 | SOME tau => tau
		     )
		   | RowType(rho) =>
			ref(RowType(cloneRow rho))

		   | FunType(tau1,tau2) =>
			ref(FunType(clone tau1, clone tau2))

		   | ConsType(taus,t) =>
		     let
			val taus2 = List.map clone taus
		     in
			case TyNameMap.find(phi, t)
			  of NONE       => ref(ConsType(taus2,t))
			   | SOME theta =>
			     let
				val (alphas,tau1) = renameTypeFcn theta
				val cloned'       = !cloned
			     in
				mu' := ListPair.foldlEq
					(fn(alpha,tau2,mu) =>
					    TyVarMap.insert(mu,alpha,tau2))
					(!mu') (alphas,taus2) ;
				clone' tau1
				before cloned := cloned'
			     end
		     end

		   (* [RFC: First-class modules] *)
		   | PackType(Sigma) =>
		     (* The signatures in package types are never inferred and
		      * signatures may not contain free type variables. Hence
		      * it suffices to simply perform the realisation and ignore
		      * inner sharing.
		      *)
			ref(PackType(!Sig.realise phi Sigma))

		   | Undetermined{stamp,...} =>
		     (case StampMap.find(det, stamp)
		        of SOME tau1 => tau1
			 | NONE      => tau
		     )
		   | Overloaded(O) =>
			tau

		   | Determined(tau) =>
			clone tau

	    and cloneRow' rho =
		case !rho
		  of FixedRow fixed =>
			 ref(FixedRow(LabMap.map clone' fixed))

		   | FlexRow{fixed, flex} =>
			 ref(FlexRow{fixed = LabMap.map clone' fixed,
				     flex = cloneRow flex})

		   | FreeRow _ =>
			 rho
	in
	    clone tau
	end

    and renameTypeFcn (alphas,tau) =
	let
	    val alphas' = List.map (TyVar.invent o TyVar.admitsEquality) alphas
	    val taus    = List.map fromTyVar alphas'
	    val mu      = TyVarMap.fromList(ListPair.zipEq(alphas, taus))
	in
	    ( alphas', substitute mu tau )
	end


    (* Substitution, realisation [Section 5.2] and determination *)

    and substitute mu = clone(mu, TyNameMap.empty, StampMap.empty)
    fun realise phi   = clone(TyVarMap.empty, phi, StampMap.empty)
    fun determine det = clone(TyVarMap.empty, TyNameMap.empty, det)


    (* Type variable and type name extraction [Section 4.2] *)

    fun tyvars(ref tau') = tyvars' tau'

    and tyvars'(TyVar(alpha)) =
	    TyVarSet.singleton alpha

      | tyvars'(RowType rho) =
	    tyvarsRow rho

      | tyvars'(FunType(tau1,tau2)) =
	    TyVarSet.union(tyvars tau1, tyvars tau2)
 
      | tyvars'(ConsType(taus,t)) =
	    List.foldl (fn(tau,U) => TyVarSet.union(U, tyvars tau))
		       TyVarSet.empty taus

      (* [RFC: First-class modules] *)
      | tyvars'(PackType(Sigma)) =
	    !Sig.tyvars Sigma

      | tyvars'(Undetermined{...}) =
	    (* Not quite right, but we never fill in types containing tyvars. *)
	    TyVarSet.empty

      | tyvars'(Overloaded(O)) =
	    TyVarSet.empty

      | tyvars'(Determined(tau)) =
	    tyvars tau

    (* [RFC: Record extension] *)
    and tyvarsRow(ref rho') = tyvarsRow' rho'

    and tyvarsRow'(FixedRow fixed) =
	    LabMap.foldl (fn(tau,U) => TyVarSet.union(U, tyvars tau))
			 TyVarSet.empty fixed

      | tyvarsRow'(FlexRow{fixed, flex}) =
	    LabMap.foldl (fn(tau,U) => TyVarSet.union(U, tyvars tau))
			 (tyvarsRow flex) fixed

      | tyvarsRow'(FreeRow _) =
	    (* Not quite right, but we never fill in types containing tyvars. *)
	    TyVarSet.empty


    fun tynames(ref tau') = tynames' tau'

    and tynames'(TyVar(alpha)) =
	    TyNameSet.empty

      | tynames'(RowType rho) =
	    tynamesRow rho

      | tynames'(FunType(tau1,tau2)) =
	    TyNameSet.union(tynames tau1, tynames tau2)
 
      | tynames'(ConsType(taus,t)) =
	let
	    val T = List.foldl (fn(tau,T) => TyNameSet.union(T, tynames tau))
			       TyNameSet.empty taus
	in
	    TyNameSet.add(T, t)
	end

      (* [RFC: First-class modules] *)
      | tynames'(PackType(Sigma)) =
	    !Sig.tynames Sigma

      | tynames'(Undetermined{...}) =
	    (* Not quite right, but currently it is OK for all uses of
	     * of this function in HaMLet. :-P *)
	    TyNameSet.empty

      | tynames'(Overloaded(O)) =
	    (* Approximation *)
	    OverloadingClass.set O

      | tynames'(Determined(tau)) =
	    tynames tau

    (* [RFC: Record extension] *)
    and tynamesRow(ref rho') = tynamesRow' rho'

    and tynamesRow'(FixedRow fixed) =
	    LabMap.foldl (fn(tau,T) => TyNameSet.union(T, tynames tau))
			 TyNameSet.empty fixed

      | tynamesRow'(FlexRow{fixed, flex}) =
	    LabMap.foldl (fn(tau,T) => TyNameSet.union(T, tynames tau))
			 (tynamesRow flex) fixed

      | tynamesRow'(FreeRow _) =
	    (* Not quite right, but currently it is OK for all uses of
	     * of this function in HaMLet. :-P *)
	    TyNameSet.empty


    fun undetermined(ref tau') = undetermined' tau'

    and undetermined'(TyVar(alpha)) =
	    StampMap.empty

      | undetermined'(RowType rho) =
	    undeterminedRow rho

      | undetermined'(FunType(tau1,tau2)) =
	    StampMap.unionWith #2 (undetermined tau1, undetermined tau2)

      | undetermined'(ConsType(taus,t)) =
	    List.foldl (fn(tau,Z) =>
		StampMap.unionWith #2 (Z, undetermined tau)) StampMap.empty taus

      (* [RFC: First-class modules] *)
      | undetermined'(PackType(Sigma)) =
	    !Sig.undetermined Sigma

      | undetermined'(Undetermined{stamp,eq,...}) =
	    StampMap.singleton(stamp, eq)

      | undetermined'(Overloaded(O)) =
	    StampMap.empty

      | undetermined'(Determined(tau)) =
	    undetermined tau

    (* [RFC: Record extension] *)
    and undeterminedRow(ref rho') = undeterminedRow' rho'

    and undeterminedRow'(FixedRow fixed) =
	    LabMap.foldl (fn(tau,Z) =>
			  StampMap.unionWith #2 (Z, undetermined tau))
			 StampMap.empty fixed

      | undeterminedRow'(FlexRow{fixed, flex}) =
	    LabMap.foldl (fn(tau,Z) =>
			  StampMap.unionWith #2 (Z, undetermined tau))
			 (undeterminedRow flex) fixed

      | undeterminedRow'(FreeRow _) =
	    StampMap.empty


    (* Check for equality type [Section 4.4] *)

    fun admitsEquality(ref tau') = admitsEquality' tau'

    and admitsEquality'(TyVar alpha) =
	    TyVar.admitsEquality alpha

      | admitsEquality'(RowType rho) =
	    admitsEqualityRow rho

      | admitsEquality'(FunType _) = false

      | admitsEquality'(ConsType(taus,t)) =
	TyName.admitsEquality t andalso List.all admitsEquality taus
	orelse TyName.toString t = "ref"

      (* [RFC: First-class modules] *)
      | admitsEquality'(PackType(Sigma)) = true

      | admitsEquality'(Undetermined{eq,...}) =
	    eq orelse raise Fail "Type.admitsEquality: undetermined type"

      | admitsEquality'(Overloaded(O)) =
	    raise Fail "Type.admitsEquality: overloaded type"

      | admitsEquality'(Determined(tau)) =
	    admitsEquality tau

    (* [RFC: Record extension] *)
    and admitsEqualityRow(ref rho') = admitsEqualityRow' rho'

    and admitsEqualityRow'(FixedRow fixed) =
	    LabMap.all admitsEquality fixed

      | admitsEqualityRow'(FlexRow{fixed, flex}) =
	    LabMap.all admitsEquality fixed andalso admitsEqualityRow flex

      | admitsEqualityRow'(FreeRow{eq,...}) =
	    eq orelse raise Fail "Type.admitsEquality: undetermined row type"


    (* Equality *)

    fun equals(ref(Determined(tau1)), tau2) = equals(tau1, tau2)
      | equals(tau1, ref(Determined(tau2))) = equals(tau1, tau2)

      | equals(tau1 as ref tau1', tau2 as ref tau2') =
	    tau1 = tau2 orelse equals'(tau1',tau2')

    and equals'(TyVar(alpha1), TyVar(alpha2)) =
	   alpha1 = alpha2

      | equals'(FunType(tau11,tau12), FunType(tau21,tau22)) =
	   equals(tau11,tau21) andalso equals(tau12,tau22)

      | equals'(RowType rho1, RowType rho2) =
	    (normalizeRow rho1; normalizeRow rho2;
	     equalsRow(rho1, rho2))

      | equals'(tau' as ConsType(taus1,t1), ConsType(taus2,t2)) =
	    t1 = t2 andalso ListPair.allEq equals (taus1,taus2)

      (* [RFC: First-class modules] *)
      | equals'(PackType(Sigma1), PackType(Sigma2)) =
	   !Sig.matches(Sigma1,Sigma2) andalso !Sig.matches(Sigma2,Sigma1)

      | equals'(Undetermined{stamp=z1,...}, Undetermined{stamp=z2,...}) =
	    raise Fail "Type.equals: undetermined"

      | equals'(Overloaded(O1), Overloaded(O2)) =
	   raise Fail "Type.equals: overloaded"

      | equals' _ = false

    (* [RFC: Record extension] *)
    and equalsRow(rho1, rho2) =
	    rho1 = rho2 orelse equalsRow'(!rho1, !rho2)

    and equalsRow'(FixedRow fixed1, FixedRow fixed2) =
	let
	    fun equalsField(lab, tau1) =
		case LabMap.find(fixed2, lab)
		  of SOME tau2 => equals(tau1,tau2)
		   | NONE      => false
	in
	    LabMap.numItems fixed1 = LabMap.numItems fixed2 andalso
	    LabMap.alli equalsField fixed1
	end

      | equalsRow'(FlexRow{fixed=fixed1, flex=flex1},
		   FlexRow{fixed=fixed2, flex=flex2}) =
	let
	    fun equalsField(lab, tau1) =
		case LabMap.find(fixed2, lab)
		  of SOME tau2 => equals(tau1,tau2)
		   | NONE      => false
	in
	    LabMap.numItems fixed1 = LabMap.numItems fixed2 andalso
	    LabMap.alli equalsField fixed1 andalso
	    equalsRow(flex1, flex2)
	end

      | equalsRow'(FreeRow _, FreeRow _) =
	    raise Fail "Type.equalsRow: undetermined"

      | equalsRow' _ = false


    (* Unification *)

    exception Unify


    fun occurs(z, ref tau') = occurs'(z, tau')

    and occurs'(z, TyVar(alpha)) =
	    false
      | occurs'(z, RowType rho) =
	    occursRow(z, rho)
      | occurs'(z, FunType(tau1,tau2)) =
	    occurs(z, tau1) orelse occurs(z, tau2)
      | occurs'(z, ConsType(taus,t)) =
	    List.exists (fn tau => occurs(z, tau)) taus
      | occurs'(z, PackType(Sigma)) =
	    (* [RFC: First-class modules] *)
	    StampMap.inDomain(!Sig.undetermined Sigma, z)
      | occurs'(z, Undetermined{stamp,...}) =
	    stamp = z
      | occurs'(z, Overloaded(O)) =
	    false
      | occurs'(z, Determined(tau)) =
	    occurs(z, tau)

    (* [RFC: Record extension] *)
    and occursRow(z, ref rho') = occursRow'(z, rho')

    and occursRow'(z, FixedRow fixed) =
	    LabMap.exists (fn tau => occurs(z, tau)) fixed
      | occursRow'(z, FlexRow{fixed, flex}) =
	    LabMap.exists (fn tau => occurs(z, tau)) fixed orelse
	    occursRow(z, flex)
      | occursRow'(z, FreeRow _) =
	    false


    fun unify(ref(Determined(tau1)), tau2) = unify(tau1, tau2)
      | unify(tau1, ref(Determined(tau2))) = unify(tau1, tau2)

      | unify(tau1 as ref tau1', tau2 as ref tau2') =
	    if tau1 = tau2 then () else
	    ( tau1 := unify'(tau1',tau2')
	    ; tau2 := Determined(tau1)
	    )

    and unify'(Undetermined{stamp,eq,time}, tau') =
	    unifyUndetermined(stamp, eq, time, tau')
      | unify'(tau', Undetermined{stamp,eq,time}) =
	    unifyUndetermined(stamp, eq, time, tau')
      | unify'(Overloaded(O), tau')     = unifyOverloaded(O, tau')
      | unify'(tau', Overloaded(O))     = unifyOverloaded(O, tau')

      | unify'(tau' as TyVar(alpha1), TyVar(alpha2)) =
	if alpha1 = alpha2 then
	    tau'
	else
	    raise Unify

      | unify'(tau' as FunType(tau11,tau12), FunType(tau21,tau22)) =
	   ( unify(tau11,tau21)
	   ; unify(tau12,tau22)
	   ; tau'
	   )

      (* [RFC: First-class modules] *)
      | unify'(tau' as PackType(Sigma1), PackType(Sigma2)) =
	(* match performs inner unification! *)
	if !Sig.matches(Sigma1,Sigma2) andalso !Sig.matches(Sigma2,Sigma1) then
	    tau'
	else
	    raise Unify

      | unify'(tau' as RowType rho1, RowType rho2) =
	   ( unifyRow(rho1, rho2)
	   ; tau'
	   )
      | unify'(tau' as ConsType(taus1,t1), ConsType(taus2,t2)) =
	if t1 = t2 then
	    ( ListPair.appEq unify (taus1,taus2)
	    ; tau'
	    )
	else
	    raise Unify

      | unify' _ = raise Unify

    (* [RFC: Record extension] *)
    and unifyRow(rho1, rho2) =
	let
	    fun split rho =
		case !rho
		  of FixedRow fixed       => (fixed, NONE)
		   | FlexRow{fixed, flex} => (fixed, SOME flex)
		   | FreeRow _            => (LabMap.empty, SOME rho)

	    val _ = normalizeRow rho1
	    val _ = normalizeRow rho2
	    val (fixed1, flex1) = split rho1
	    val (fixed2, flex2) = split rho2

	    fun unifyFixed flex (lab, tau1, fixed) =
		case LabMap.find(fixed, lab)
		  of SOME tau2 => ( unify(tau1,tau2)
				  ; #1(LabMap.remove(fixed, lab))
				  )
		   | NONE =>
		case flex
		  of SOME(ref(FreeRow{eq, time, excluded})) =>
		     if LabSet.member(excluded, lab) then
			 raise Unify
		     else
		         ( propagate (time, eq) tau1 ; fixed )
		   | _ => raise Unify

	    val fixed1' = LabMap.foldli (unifyFixed flex1) fixed1 fixed2
	    val fixed2' = LabMap.foldli (unifyFixed flex2) fixed2 fixed1
	    val fixed'  = LabMap.unionWith #2 (fixed1, fixed2)
	    val flex' =
		case (flex1, flex2)
		  of (SOME(ref(FreeRow r1)), SOME(ref(FreeRow r2))) =>
		      SOME(ref(FreeRow{eq = #eq r1 orelse #eq r2,
				       time = Stamp.min(#time r1, #time r2),
				       excluded = LabSet.union(#excluded r1,
							       #excluded r2)}))
		   | _ => NONE

	    fun unifyFlex fixed flex =
		flex := (case flex'
			   of NONE       => FixedRow fixed
			    | SOME flex' => FlexRow{fixed=fixed, flex=flex'})
	in
	    Option.map (unifyFlex fixed2') flex1;
	    Option.map (unifyFlex fixed1') flex2;
	    unifyFlex fixed' rho1;
	    unifyFlex fixed' rho2
	end

    and unifyUndetermined(z, eq, time, tau') =
	if occurs'(z, tau') then
	    raise Unify
	else
	    propagate'(time, eq) tau'

    and unifyOverloaded(O, Undetermined{stamp,eq,time}) =
	    unifyUndetermined(stamp, eq, time, Overloaded(O))

      | unifyOverloaded(O, tau' as ConsType([],t)) =
	if OverloadingClass.member(O, t) then
	    tau'
	else
	    raise Unify

      | unifyOverloaded(O1, Overloaded(O2)) =
	(case OverloadingClass.intersection(O1,O2)
	   of NONE   => raise Unify
	    | SOME O => Overloaded(O)
	)
      | unifyOverloaded(O, _) =
	    raise Unify


    and propagate (time, eq) (tau as ref tau') =
	    tau := propagate'(time, eq) tau'

    and propagate'(time, eq) (tau' as TyVar(alpha)) =
	if not eq orelse TyVar.admitsEquality alpha then
	    tau'
	else
	    raise Unify

      | propagate'(time, eq) (tau' as RowType rho) =
	    ( propagateRow (time, eq) rho
	    ; tau'
	    )
      | propagate'(time, eq) (tau' as FunType(tau1,tau2)) =
	if eq then
	    raise Unify
	else
	    ( propagate (time, eq) tau1
	    ; propagate (time, eq) tau2
	    ; tau'
	    )
      | propagate'(time, eq) (tau' as ConsType(taus,t)) =
	(case Stamp.compare(TyName.time t, time)
	   of GREATER => raise Unify
	    | _ =>
	      if not eq orelse TyName.toString t = "ref" then
		  ( List.app (propagate(time, false)) taus ; tau' )
	      else if TyName.admitsEquality t then
		  ( List.app (propagate(time, eq)) taus ; tau' )
	      else
		  raise Unify
	)
      (* [RFC: First-class modules] *)
      | propagate'(time, eq) (tau' as PackType(Sigma)) =
	(* Package types always admit equality, and the contained 
	 * signature is never inferred. Hence, it suffices to
	 * consider the time stamps of the contained type names.
	 *)
	    ( TyNameSet.app (fn t =>
			     if Stamp.compare(TyName.time t, time) = GREATER
			     then raise Unify
			     else ()) (!Sig.tynames Sigma)
	    ; tau'
	    )
      | propagate'(time, eq) (Undetermined{stamp=z, eq=eq', time=time'}) =
	    Undetermined{stamp = z, eq = eq orelse eq',
			 time = Stamp.min(time, time')}

      | propagate'(time, eq) (tau' as Overloaded(O)) =
	if not eq then tau' else
	(case OverloadingClass.makeEquality O
	   of NONE    => raise Unify
	    | SOME O' => Overloaded(O')
	)
      | propagate'(time, eq) (tau' as Determined(tau)) =
	    ( propagate (time, eq) tau ; tau' )

    (* [RFC: Record extension] *)
    and propagateRow (time, eq) (rho as ref rho') =
	    rho := propagateRow'(time, eq) rho'

    and propagateRow'(time, eq) (rho' as FixedRow fixed) =
	    ( LabMap.app (propagate(time, eq)) fixed
	    ; rho'
	    )
      | propagateRow'(time, eq) (rho' as FlexRow{fixed, flex}) =
	    ( LabMap.app (propagate(time, eq)) fixed
	    ; propagateRow (time, eq) flex
	    ; rho'
	    )
      | propagateRow'(time, eq) (FreeRow{eq=eq', time=time', excluded}) =
	    FreeRow{eq = eq orelse eq', time = Stamp.min(time, time'),
		    excluded = excluded}


    (* Assign default to overloaded type and check for remaining
     * flexible types [Appendix E and Section 4.11] *)

    exception Flexible

    fun resolve(ref(Determined tau)) =
	    resolve tau

      | resolve(tau as ref(Overloaded(O))) =
	    tau := ConsType([], OverloadingClass.default O)

      | resolve(ref(RowType rho)) =
	    resolveRow rho

      | resolve _ = ()

    (* [RFC: Record extension] *)
    and resolveRow(ref(FlexRow{flex, ...})) =
	    resolveRow flex

      | resolveRow(ref(FreeRow _)) =
	    raise Flexible

      | resolveRow _ = ()
end;
