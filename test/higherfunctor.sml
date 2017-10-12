signature GG = fct (type t type u=int) -> (type t'=t type u'=u type v=int type w);
signature HH = (type int structure G : fct (type t type u=int) -> (type t'=t type u'=u type v=int type w));

(* Should match *)
functor F(X:GG) = X:fct (type t type u=int) -> (type t'=t type u'=u type v=int type w)
functor F(X:GG) = X:fct (type t=int type u=int) -> (type t'=t type u'=u type v=int type w)
functor F(X:GG) = X:fct (type t=int type u=t) -> (type t'=t type u'=u type v=int type w)
functor F(X:GG) = X:fct (type t type u=int) -> (type t'=t type u'=int type v=int type w)
functor F(X:GG) = X:fct (type t=int type u=t) -> (type t'=t type u'=t' type v=u' type w)
functor F(X:GG) = X:fct (type t type u=int) -> (type t' type u' type v type w)
functor F(X:GG) = X:fct (type t type u=int) -> ()
;
functor F(X:HH) = X:(type int=X.int structure G : fct (type t type u=int) -> (type t'=t type u'=u type v=int type w))
functor F(X:HH) = X:(type int=X.int structure G : fct (type t=int type u=int) -> (type t'=t type u'=u type v=int type w))
functor F(X:HH) = X:(type int=X.int structure G : fct (type t=int type u=t) -> (type t'=t type u'=u type v=int type w))
functor F(X:HH) = X:(type int=X.int structure G : fct (type t type u=int) -> (type t'=t type u'=int type v=int type w))
functor F(X:HH) = X:(type int=X.int structure G : fct (type t=int type u=t) -> (type t'=t type u'=t' type v=u' type w))
functor F(X:HH) = X:(type int=X.int structure G : fct (type t type u=int) -> (type t' type u' type v type w))
functor F(X:HH) = X:(type int=X.int structure G : fct (type t type u=int) -> ())
functor F(X:HH) = X:(type int structure G : fct (type t type u=int) -> (type t'=t type u'=u type v=int type w))
functor F(X:HH) = X:(type int structure G : fct (type t=int type u=int) -> (type t'=t type u'=u type v=int type w))
functor F(X:HH) = X:(type int structure G : fct (type t=int type u=t) -> (type t'=t type u'=u type v=int type w))
functor F(X:HH) = X:(type int structure G : fct (type t type u=int) -> (type t'=t type u'=int type v=int type w))
functor F(X:HH) = X:(type int structure G : fct (type t=int type u=t) -> (type t'=t type u'=t' type v=u' type w))
functor F(X:HH) = X:(type int structure G : fct (type t type u=int) -> (type t' type u' type v type w))
functor F(X:HH) = X:(type int structure G : fct (type t type u=int) -> ())
;

(* Should not match *)
functor F(X:GG) = X:fct (type t type u=int) -> (type t'=t type u'=u type v=int type w=int);
functor F(X:GG) = X:fct (type t type u) -> (type t'=t type u'=int type v=int type w);
functor F(X:GG) = X:fct (type t) -> (type t'=t type u'=int type v=int type w);
functor F(X:GG) = X:fct () -> (type t' type u' type v=int type w);
