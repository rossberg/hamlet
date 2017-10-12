(*
 * (c) Andreas Rossberg 1999-2007
 *
 * Standard ML functor signatures
 *
 * Definition, Sections 5.1 and 5.4
 * + RFC: Higher-order functors
 *)

structure FunSig :> FUNSIG =
struct
    (* Import types *)

    open StaticObjectsCore
    open StaticObjectsModule


    (* Type variable and type name extraction [Section 4.2] *)

    fun tyvars (T,(M,Sigma)) =
	TyVarSet.union(StaticEnv.tyvarsM M, Sig.tyvars Sigma)

    fun tynames (T,(M,Sigma)) =
	TyNameSet.difference(TyNameSet.union(StaticEnv.tynamesM M,
					     Sig.tynames Sigma), T)
    fun undetermined (T,(M,Sigma)) =
	StampMap.unionWith #2 (StaticEnv.undeterminedM M, Sig.undetermined Sigma)

    (* Alpha Renaming *)

    fun rename (T,(M,Sigma)) =
	let
	    val phi' = TyNameSet.foldl
			 (fn(t,phi')=> TyNameMap.insert(phi',t,TyName.rename t))
			 TyNameMap.empty T
	    val phi = TyNameMap.map (TypeFcn.rename o TypeFcn.fromTyName) phi'
	    val T'  = TyNameSet.map (fn t => valOf(TyNameMap.find(phi',t))) T
	    val M'  = StaticEnv.realiseM phi M
	    val Sigma' = Sig.realise phi Sigma
	in
	    (T',(M',Sigma'))
	end

    (* Matching [Section 5.6; RFC: Higher-order functors] *)

    fun matches(Phi1, (T2,(M2,Sigma2))) =
	let
	    val (T1,(M1,Sigma1)) = rename Phi1
	    val (Mminus,phi) = Sig.match(M2, (T1,M1))
	in
	    Sig.matches(Sig.realise phi Sigma1, Sigma2)
	end
	handle Sig.Match => false


    (* Tie recursive imports *)

    val _ = StaticEnv.FunSig.matches := matches
end;
