(* binary-map-fn.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This code was adapted from Stephen Adams' binary tree implementation
 * of applicative integer sets.
 *
 *   Copyright 1992 Stephen Adams.
 *
 *    This software may be used freely provided that:
 *      1. This copyright notice is attached to any copy, derived work,
 *         or work including all or part of this software.
 *      2. Any derived work must contain a prominent notice stating that
 *         it has been altered from the original.
 *
 *
 *   Name(s): Stephen Adams.
 *   Department, Institution: Electronics & Computer Science,
 *      University of Southampton
 *   Address:  Electronics & Computer Science
 *             University of Southampton
 *	     Southampton  SO9 5NH
 *	     Great Britian
 *   E-mail:   sra@ecs.soton.ac.uk
 *
 *   Comments:
 *
 *     1.  The implementation is based on Binary search trees of Bounded
 *         Balance, similar to Nievergelt & Reingold, SIAM J. Computing
 *         2(1), March 1973.  The main advantage of these trees is that
 *         they keep the size of the tree in the node, giving a constant
 *         time size operation.
 *
 *     2.  The bounded balance criterion is simpler than N&R's alpha.
 *         Simply, one subtree must not have more than `weight' times as
 *         many elements as the opposite subtree.  Rebalancing is
 *         guaranteed to reinstate the criterion for weight>2.23, but
 *         the occasional incorrect behaviour for weight=2 is not
 *         detrimental to performance.
 *
 *)

functor BinaryMapFn (K : ORD_KEY) : ORD_MAP =
  struct

    structure Key = K

    (*
    **  val weight = 3
    **  fun wt i = weight * i
    *)
    fun wt (i : int) = i + i + i

    datatype 'a map
      = E 
      | T of {
          key : K.ord_key, 
          value : 'a, 
          cnt : int, 
          left : 'a map, 
          right : 'a map
	}

    val empty = E

    fun isEmpty E = true
      | isEmpty _ = false

    fun numItems E = 0
      | numItems (T{cnt,...}) = cnt

  (* return the first item in the map (or NONE if it is empty) *)
    fun first E = NONE
      | first (T{value, left=E, ...}) = SOME value
      | first (T{left, ...}) = first left

  (* return the first item in the map and its key (or NONE if it is empty) *)
    fun firsti E = NONE
      | firsti (T{key, value, left=E, ...}) = SOME(key, value)
      | firsti (T{left, ...}) = firsti left

local
    fun N(k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | N(k,v,E,r as T n) = T{key=k,value=v,cnt=1+(#cnt n),left=E,right=r}
      | N(k,v,l as T n,E) = T{key=k,value=v,cnt=1+(#cnt n),left=l,right=E}
      | N(k,v,l as T n,r as T n') = 
          T{key=k,value=v,cnt=1+(#cnt n)+(#cnt n'),left=l,right=r}

    fun single_L (a,av,x,T{key=b,value=bv,left=y,right=z,...}) = 
          N(b,bv,N(a,av,x,y),z)
      | single_L _ = raise Match
    fun single_R (b,bv,T{key=a,value=av,left=x,right=y,...},z) = 
          N(a,av,x,N(b,bv,y,z))
      | single_R _ = raise Match
    fun double_L (a,av,w,T{key=c,value=cv,left=T{key=b,value=bv,left=x,right=y,...},right=z,...}) =
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_L _ = raise Match
    fun double_R (c,cv,T{key=a,value=av,left=w,right=T{key=b,value=bv,left=x,right=y,...},...},z) = 
          N(b,bv,N(a,av,w,x),N(c,cv,y,z))
      | double_R _ = raise Match

    fun T' (k,v,E,E) = T{key=k,value=v,cnt=1,left=E,right=E}
      | T' (k,v,E,r as T{right=E,left=E,...}) =
          T{key=k,value=v,cnt=2,left=E,right=r}
      | T' (k,v,l as T{right=E,left=E,...},E) =
          T{key=k,value=v,cnt=2,left=l,right=E}

      | T' (p as (_,_,E,T{left=T _,right=E,...})) = double_L p
      | T' (p as (_,_,T{left=E,right=T _,...},E)) = double_R p

        (* these cases almost never happen with small weight*)
      | T' (p as (_,_,E,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...})) =
          if ln < rn then single_L p else double_L p
      | T' (p as (_,_,T{left=T{cnt=ln,...},right=T{cnt=rn,...},...},E)) =
          if ln > rn then single_R p else double_R p

      | T' (p as (_,_,E,T{left=E,...})) = single_L p
      | T' (p as (_,_,T{right=E,...},E)) = single_R p

      | T' (p as (k,v,l as T{cnt=ln,left=ll,right=lr,...},
                      r as T{cnt=rn,left=rl,right=rr,...})) =
          if rn >= wt ln then (*right is too big*)
            let val rln = numItems rl
                val rrn = numItems rr
            in
              if rln < rrn then  single_L p  else  double_L p
            end
        
          else if ln >= wt rn then  (*left is too big*)
            let val lln = numItems ll
                val lrn = numItems lr
            in
              if lrn < lln then  single_R p  else  double_R p
            end
    
          else T{key=k,value=v,cnt=ln+rn+1,left=l,right=r}

    local
      fun min (T{left=E,key,value,...}) = (key,value)
        | min (T{left,...}) = min left
        | min _ = raise Match
  
      fun delmin (T{left=E,right,...}) = right
        | delmin (T{key,value,left,right,...}) = T'(key,value,delmin left,right)
        | delmin _ = raise Match
    in
      fun delete' (E,r) = r
        | delete' (l,E) = l
        | delete' (l,r) = let val (mink,minv) = min r in
            T'(mink,minv,l,delmin r)
          end
    end
in
    fun mkDict () = E
    
    fun singleton (x,v) = T{key=x,value=v,cnt=1,left=E,right=E}

    fun insert (E,x,v) = T{key=x,value=v,cnt=1,left=E,right=E}
      | insert (T(set as {key,left,right,value,...}),x,v) =
          case K.compare (key,x) of
            GREATER => T'(key,value,insert(left,x,v),right)
          | LESS => T'(key,value,left,insert(right,x,v))
          | _ => T{key=x,value=v,left=left,right=right,cnt= #cnt set}
    fun insert' ((k, x), m) = insert(m, k, x)

    fun inDomain (set, x) = let 
	  fun mem E = false
	    | mem (T(n as {key,left,right,...})) = (case K.compare (x,key)
		 of GREATER => mem right
		  | EQUAL => true
		  | LESS => mem left
		(* end case *))
	  in
	    mem set
	  end

    fun find (set, x) = let 
	  fun mem E = NONE
	    | mem (T(n as {key,left,right,...})) = (case K.compare (x,key)
		 of GREATER => mem right
		  | EQUAL => SOME(#value n)
		  | LESS => mem left
		(* end case *))
	  in
	    mem set
	  end

    fun remove (E,x) = raise LibBase.NotFound
      | remove (set as T{key,left,right,value,...},x) = (
          case K.compare (key,x)
	   of GREATER => let
		val (left', v) = remove(left, x)
		in
		  (T'(key, value, left', right), v)
		end
            | LESS => let
		val (right', v) = remove (right, x)
		in
		  (T'(key, value, left, right'), v)
		end
            | _ => (delete'(left,right),value)
	  (* end case *))

    fun listItems d = let
	  fun d2l (E, l) = l
	    | d2l (T{key,value,left,right,...}, l) =
		d2l(left, value::(d2l(right,l)))
	  in
	    d2l (d,[])
	  end

    fun listItemsi d = let
	  fun d2l (E, l) = l
	    | d2l (T{key,value,left,right,...}, l) =
		d2l(left, (key,value)::(d2l(right,l)))
	  in
	    d2l (d,[])
	  end

    fun listKeys d = let
	  fun d2l (E, l) = l
	    | d2l (T{key,left,right,...}, l) = d2l(left, key::(d2l(right,l)))
	  in
	    d2l (d,[])
	  end

    local
      fun next ((t as T{right, ...})::rest) = (t, left(right, rest))
	| next _ = (E, [])
      and left (E, rest) = rest
	| left (t as T{left=l, ...}, rest) = left(l, t::rest)
    in
    fun collate cmpRng (s1, s2) = let
	  fun cmp (t1, t2) = (case (next t1, next t2)
		 of ((E, _), (E, _)) => EQUAL
		  | ((E, _), _) => LESS
		  | (_, (E, _)) => GREATER
		  | ((T{key=x1, value=y1, ...}, r1), (T{key=x2, value=y2, ...}, r2)) => (
		      case Key.compare(x1, x2)
		       of EQUAL => (case cmpRng(y1, y2)
			     of EQUAL => cmp (r1, r2)
			      | order => order
			    (* end case *))
			| order => order
		      (* end case *))
		(* end case *))
	  in
	    cmp (left(s1, []), left(s2, []))
	  end
    end (* local *)

    fun appi f d = let
	  fun app' E = ()
	    | app' (T{key,value,left,right,...}) = (
		app' left; f(key, value); app' right)
	  in
	    app' d
	  end
    fun app f d = let
	  fun app' E = ()
	    | app' (T{value,left,right,...}) = (
		app' left; f value; app' right)
	  in
	    app' d
	  end

    fun mapi f d = let
	  fun map' E = E
	    | map' (T{key,value,left,right,cnt}) = let
		val left' = map' left
		val value' = f(key, value)
		val right' = map' right
		in
		  T{cnt=cnt, key=key, value=value', left = left', right = right'}
		end
	  in
	    map' d
	  end
    fun map f d = mapi (fn (_, x) => f x) d

    fun foldli f init d = let
	  fun fold (E, v) = v
	    | fold (T{key,value,left,right,...}, v) =
		fold (right, f(key, value, fold(left, v)))
	  in
	    fold (d, init)
	  end
    fun foldl f init d = foldli (fn (_, v, accum) => f (v, accum)) init d

    fun foldri f init d = let
	  fun fold (E,v) = v
	    | fold (T{key,value,left,right,...},v) =
		fold (left, f(key, value, fold(right, v)))
	  in
	    fold (d, init)
	  end
    fun foldr f init d = foldri (fn (_, v, accum) => f (v, accum)) init d

(** To be implemented **
    val filter  : ('a -> bool) -> 'a map -> 'a map
    val filteri : (Key.ord_key * 'a -> bool) -> 'a map -> 'a map
**)

    end (* local *)

(* the following are generic implementations of the unionWith and intersectWith
 * operetions.  These should be specialized for the internal representations
 * at some point.
 *)
    fun unionWith f (m1, m2) = let
	  fun ins  f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (a, b) => f (b, a))) m1 m2
	      else foldli (ins f) m2 m1
	  end
    fun unionWithi f (m1, m2) = let
	  fun ins f (key, x, m) = (case find(m, key)
		 of NONE => insert(m, key, x)
		  | (SOME x') => insert(m, key, f(key, x, x'))
		(* end case *))
	  in
	    if (numItems m1 > numItems m2)
	      then foldli (ins (fn (k, a, b) => f (k, b, a))) m1 m2
	      else foldli (ins f) m2 m1
	  end

    fun intersectWith f (m1, m2) = let
	(* iterate over the elements of m1, checking for membership in m2 *)
	  fun intersect f (m1, m2) = let
		fun ins (key, x, m) = (case find(m2, key)
		       of NONE => m
			| (SOME x') => insert(m, key, f(x, x'))
		      (* end case *))
		in
		  foldli ins empty m1
		end
	  in
	    if (numItems m1 > numItems m2)
	      then intersect f (m1, m2)
	      else intersect (fn (a, b) => f(b, a)) (m2, m1)
	  end
    fun intersectWithi f (m1, m2) = let
	(* iterate over the elements of m1, checking for membership in m2 *)
	  fun intersect f (m1, m2) = let
		fun ins (key, x, m) = (case find(m2, key)
		       of NONE => m
			| (SOME x') => insert(m, key, f(key, x, x'))
		      (* end case *))
		in
		  foldli ins empty m1
		end
	  in
	    if (numItems m1 > numItems m2)
	      then intersect f (m1, m2)
	      else intersect (fn (k, a, b) => f(k, b, a)) (m2, m1)
	  end

  (* this is a generic implementation of filter.  It should
   * be specialized to the data-structure at some point.
   *)
    fun filter predFn m = let
	  fun f (key, item, m) = if predFn item
		then insert(m, key, item)
		else m
	  in
	    foldli f empty m
	  end
    fun filteri predFn m = let
	  fun f (key, item, m) = if predFn(key, item)
		then insert(m, key, item)
		else m
	  in
	    foldli f empty m
	  end

  (* this is a generic implementation of mapPartial.  It should
   * be specialized to the data-structure at some point.
   *)
    fun mapPartial f m = let
	  fun g (key, item, m) = (case f item
		 of NONE => m
		  | (SOME item') => insert(m, key, item')
		(* end case *))
	  in
	    foldli g empty m
	  end
    fun mapPartiali f m = let
	  fun g (key, item, m) = (case f(key, item)
		 of NONE => m
		  | (SOME item') => insert(m, key, item')
		(* end case *))
	  in
	    foldli g empty m
	  end

  end (* functor BinaryMapFn *)
;
