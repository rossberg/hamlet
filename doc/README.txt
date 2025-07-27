This version of HaMLet incorporates a number of simple extensions and
changes proposed for Successor ML [www.successor-ml.org].


Lexical Syntax
--------------

* Line Comments.

  The token "(*)" begins a comment that stretches until the end of line.

* Extended Literals.

  Numeric literals may contain underscores to group digits, as in
  1_000_000_000, 3.141_592_653, or 0wx_f300_4588. Furthermore, numeric
  literals in binary notation are supported, e.g. 0wb1101_0010.


Records
-------

* Record Punning (derived form).

  In record expressions, a field of the form id=id can be abbreviated
  to id (in Standard ML that is only allowed in patterns). For instance:

    fn {a, b, c} => {a, b=b+1, c, d=0}

* Record Extension.

  A record r can be extended using the notation {a=2, b=3, ...=r}. For
  instance,

    val r1 = {a=1, b=2}
    val r2 = {c=0, ...=r1}

  binds r2 to {a=1, b=2, c=0}. The same syntax is available in patterns
  to match the "rest" of a record:

    case r2 of {b, ...=r'} => r'

  evaluates to {a=1, c=0}.

  As a derived form, ellipses can appear at any position in the record
  (but only once):

    val r2 = {...=r1, c=0}

  Note that the context must still determine the fields denoted by the
  ellipses (i.e. there is no record polymorphism yet). Those fields may
  not overlap with the enumerated ones.

  Record types can also be formed by extension:

    type 'a t = {a:'a, b:bool}
    type 'a u = {c:char, d:'a list, ...:'a t}

  Now 'a u is equivalent to {a:'a, b:bool, c:char, d:'a list}.

* Record update (derived form).

  Besides extension, a record r can be updated, using the notation
  {r where a=2, b=3}. For example:

    val r1 = {a=1, b=2, c=6}
    val r2 = {r1 where b="hi"}

  binds r2 to {a=1, b="hi", c=6}.

  Again, the context must determine the fields in the base record.
  It must include all updated fields (i.e. it is orthogonal to extension).

  Note that update may change the types of modified fields.


Pattern Matching
----------------

* Disjunctive Patterns.

  So-called "or-patterns" of the form "pat1 | pat2" are supported. For
  example:

    fun fac (0|1) = 1
      | fac   n   = n * fac(n-1)

  The syntax subsumes multiple or-patterns and multiple matches:

    case exp of
      | A | B | C => 1
      | D | E     => 2

* Conjunctive Patterns.

  Layered patterns ("as" patterns) have been generalised to allow arbitrary
  subpatterns on both sides, i.e. "pat1 as pat2". They are useful in
  combination with nested matches (see below), but also have the advantage that
  the order of binding may now be inverted (pat as x), which sometimes is more
  readable.

* Nested matches.

  Patterns may contain nested matching constructs of the form

    pat1 with pat2 = exp

  Such a pattern is matched by first matching pat1, then evaluating exp, and
  matching its result against pat2. Variables bound in pat1 may occur in exp.
  The pattern fails when either pattern does not match. The pattern binds the
  combined set of variables occuring in pat1 and pat2. For instance, consider:

    case xs of [x,y] with SOME z = f(x,y) => x+y+z | _ => 0

  If xs is a two-element list [x,y] such that f(x,y) returns SOME z, then the
  whole expression evaluates to x+y+z, otherwise to 0.

  Nested matches are a very general construct that allows simple "views" and
  pattern guards to be defined uniformly as derived forms (see below). They
  can also be useful in combination with disjunctive patterns,

    case args of x::_ | (nil with x = 0) => ...

  or with guards (see below):

    fun escape #"\"" = "\\\""
      | escape #"\\" = "\\\\"
      | escape (c with n=ord c) if (n < 32) = "\\^" ^ str(chr(n+64))
      | escape c = str c

* Pattern guards (derived form).

  Patterns may contain a boolean guards of the form "if exp", like in

    fun nth(l, n) =
        case (l, n) of
           | (x::_,  0)        => x
           | (_::xs, n) if n>0 => nth(xs, n-1)
           |     _             => raise Subscript

  Guards are also allowed in function declarations:

    fun nth(x::_,  0)          = x
      | nth(_::xs, n) if (n>0) = nth(xs, n-1)
      | nth(  _,   _)          = raise Subscript

  A pattern guard "pat if exp" actually is syntactic sugar for the the nested
  pattern "pat with true = exp" and may hence appear inside other patterns.

* Transformation patterns (derived form).

  Transformation patterns provide "poor man's views" in the form

    ?exp
    ?exp pat

  The first form provides boolean "views":

    fun skipSpace(?isSpace :: cs) = skipSpace cs
      | skipSpace cs = cs

  The parameterised form allows actual matching. Consider a signature for
  queues:

    type 'a queue
    val empty : 'a queue
    val enqueue : 'a * 'a queue -> 'a queue
    val dequeue : 'a queue -> ('a * 'a queue) option

  With transformations, queues can be pattern matched as follows:

    fun process (?dequeue(x,q)) = (digest x; process q)
      | process _ = terminate()

  The transformation may be denoted by an arbitrary expression. Consider a
  simple set ADT:

    type set
    val empty : set
    val insert : int -> set -> set
    val isEmpty : set -> bool
    val has : int -> set -> bool

  The following is possible:

    fun f n ?isempty = f1 ()
      | f n ?(has n) = f2 ()
      | f n _ = f3 ()

  Or another example, with a parameterised transformation:

    (*) val split : char -> string -> (string * string) option

    fun manExp(?(split #"E")(b,e)) = (b,e)
      | manExp s = (s,"1")

  Transformation patterns are actually only sugar for nested matches: the
  form "?exp" expands to "x with true = exp(x)", while the parameterised form
  "?exp pat" expands to "x with SOME(pat) = exp(x)".

  As a minor subtlety, in patterns with multiple subpatterns, patterns to the
  right may refer to variables bound by patterns to the left. For example,

    (x, ?(equals x))
    x as ?(notOccurs x)(T(x1,x2))

  In particular, this allows the function f from above to be expressed without
  a separate case expression.

  Note that HaMLet also features proper views (see below). Simultaneous support
  for both these features should allow evaluating the merits of each approach.

* Optional Bars and Semicolons.

  An optional bar is allowed before the first rule of a match, yielding
  more regular and editing-friendly notation:

    case f n of
       | LESS => f(n-1)
       | EQUAL => n
       | GREATER => f(n+1)

  This is also supported for "fn" and "handle", for function declarations, and
  for datatype declarations. For instance,

    datatype 'a exp =
       | Const  of 'a
       | Var    of string
       | Lambda of string * 'a exp
       | App    of 'a exp * 'a exp

  In a similar vein, optional terminating semicolons are allowed for expression
  sequences. For example, in a let expression:

    fun myfunc1(x, y) =
        let val z = x + y in
          f x;
          g y;
          h z;
        end

  The same applies to parenthesised expressions and sequences.


Value Definitions
-----------------

* Simpler Recursive Bindings.

  Recursive bindings can no longer override constructor status.

  The syntax for recursive bindings has been made less baroque: the "rec"
  keyword must always follow directly after "val", and may no longer be
  repeated. This just rules out pathological cases most programmers might
  not even be aware of.

* Strengthened Value Restriction.

  The value restriction has been extended to demand that patterns in
  polymorphic bindings are exhaustive. This fixes a conceptual bug
  in the language and enables type passing implementations.

* Do declarations.

  The simple derived form

    do exp

  expands to "val () = exp" and allows expressions to be evaluated for their
  side effects within declarations.


Type Definitions
----------------

* Withtype Specifications (derived form).

  The "withtype" syntax for defining mutually recursive datatypes and
  type synonyms is available in signatures (in Standard ML it is only
  supported in structures).

* Proper Scoping for Transparent Type Specifications (derived form).

  Transparent type specifications in signatures have scoping rules
  consistent with the rest of the language, i.e.

    type t = bool
    signature S =
    sig
      type t = int
      and  u = t
    end

  no longer equates u with int.

* Degraded Abstype (derived form).

  The more or less obsolete "abstype" declaration form has been removed
  from the bare language and been redefined as a simple derived form.
  This change does not have much visible effect, but simplifies
  implementations and can be seen as a first step towards removing it
  altogether.

* Abolished "and" in Type Realisations (derived form).

  The syntax of type realisations using "where" does no longer allow
  multiple equations connected with "and". That arcane syntax produced
  an annoying singularity in the language and was never implemented
  correctly by most SML implementations.


Views
-----

* View Declarations.

  Views enable the definition of abstract constructors for arbitrary types that
  can be used in patterns as if they were ordinary datatype constructors. A
  view primarily defines a set of constructors and two functions for converting
  between these and the actual type the view is defined for. For example,
  consider a simple view allowing (positive) integers to be viewed as inductive
  numbers:

    viewtype peano = int as Zero | Succ of int
    with
      fun from Zero     = 0
        | from (Succ n) = n+1
      fun to 0          = Zero
        | to n if (n>0) = Succ(n-1)
        | to n          = raise Domain
    end

  This defines a view for type int. The type constructor peano provides a name
  for this view. Views may be defined for arbitrary types, and there may be
  arbitrarily many views for a given type.

  Given the viewtype definition above, we can construct integers using the
  constructors it introduces:

    val n = Succ(Succ(Succ Zero))   (*) binds n to 3
    val n = Succ 2                  (*) likewise

  The function from given with the view declaration defines how a view
  constructor is converted to the underlying type, and is applied implicitly
  for every occurrence of a view constructor in an expression.

  The inverse function to defines how a value of the underlying type is
  interpreted in terms of the view constructors. It is applied implicitly
  whenever a value of the underlying type is matched against a pattern using
  one of the view's constructors:

    fun fac Zero    = 1
      | fac(Succ n) = Succ n * fac n

  This defines a factorial function on integers. When fac is applied to an
  integer i, the function to is implicitly applied to i first and its result
  is matched against the constructors appearing in the definition of fac.

  The body of a view declaration may contain arbitrary (auxiliary)
  declarations, but must feature the two functions from and to with the
  appropriate types. None of the declarations is visible outside the view
  declaration.

  Views must be used consistently, that is, a match may not use different
  views, or a view and concrete constants of the underlying type, *for the
  same position* in a pattern. For instance, the following is illegal:

    fun fac (0 | 1)  = 1
      | fac (Succ n) = Succ n * fac n

  Thanks to this restriction, the compiler is still able to check
  exhaustiveness and irredundancy of patterns, even in the presence of views.

* View Specifications.

  Views are particularly interesting in conjunction with abstract types. For
  that purpose, it is possible to specify views in signatures:

    signature COMPLEX =
    sig
      type complex
      viewtype cart = complex as Cart of real * real
      viewtype pole = complex as Pole of real * real
    end

  A view specification can either be matched by a corresponding view
  declaration, or by an appropriate datatype definition:

    structure Complex :> COMPLEX =
    struct
      datatype cart = Cart of real * real
      type complex = cart
      viewtype pole = complex as Pole of real * real
      with
        open Math
        fun to(Cart(x,y)) = Pole(sqrt(x*x + y*y), atan2(x,y))
        fun from(Pole(r,t)) = Cart(r*cos(t), r*sin(t))
      end
   end

  The implementation of a viewtype is kept abstract, and both of the above
  views can be used uniformly where appropriate:

    open Complex
    fun add(Cart(x1,y1), Cart(x2,y2)) = Cart(x1+x2, y1+y2)
    fun mul(Pole(r1,t1), Pole(r2,t2)) = Pole(r1*r2, t1+t2)

* View Replication (derived form).

  Instead of opening the structure, a view can also be pulled into scope (and
  thus enable unqualified use of its constructors) by a viewtype replication
  declaration, analogous to SML's datatype replication:

    viewtype cart = viewtype Complex.cart

  Apart from viewtype replication, the name of a view acts as a synonym for
  the underlying representation type -- except inside the view definition
  itself, where it used to denote the (otherwise anonymous) datatype
  representing the view.


Modules
-------

* Higher-order Functors.

  To support higher-order modules, structure expressions are generalised
  to include functor expressions, analogous to function expressions in the
  core:

    fct strid : sigexp => strexp

  Likewise, signature expressions may denote dependent functor signatures:

    fct strid : sigexp1 -> sigexp2

  As a derived form, non-dependent functor signatures (where strid does
  not occur in sigexp2) may be abbreviated as follows:

    sigexp1 -> sigexp2

  SML's functor declarations are degraded to a mere derived forms,
  analogous to function declarations with "fun" in the core language. They
  support curried functors:

    functor strid (strid1 : sigexp1) ... (stridN : sigexpN) = strexp

  For uniformity, and to avoid subtle syntax, the identifier classes for
  structures and functors are merged. As another derived form, SML/NJ
  compatible syntax is provided for functor descriptions in signatures:

    functor strid (strid1 : sigexp1) ... (stridN : sigexpN) : sigexp

  Functor application syntax is generalised to

    strexp1 strexp2

  as in the core. Parentheses are allowed anywhere in structure and
  signature expressions. The derived form allowing a parenthesised
  declaration as a functor argument is maintained and generalised by
  enabling

    ( strdec )

  to abbreviate a structure in all contexts. For symmetry,

    ( spec )

  can be used to abbreviate a signature. Particularly, it can
  abbreviate a functor argument:

    fct (spec) => strexp
    fct (spec) -> sigexp

  which is also allowed in the functor declaration and specification
  derived forms, generalising the similar derived form known from SML.

  The semantics is kept simple. All functors are fully generative.

* Nested Signatures.

  Signatures are allowed as structure members. This implies the presence
  of qualified signature identifiers A.B.S, and the addition of (manifest)
  signature specifications in signatures:

    signature S =
    sig
      type t
      signature T = sig val x : t end
    end
    structure M : S =
    struct
      type t = int
      signature T = sig val x : int end
    end
    structure N : M.T = struct val x = 79 end

  A signature definition matches a signature specification if and only if
  they denote equivalent signatures.

* Local Modules.

  Structure, functor and signature declarations are allowed in local scope:

    fun sortWithoutDups compare =
        let
          structure Set = MkSet(type t = string; val compare = compare)
        in
          Set.toList o foldr Set.insert Set.empty
        end

  Naturally, types defined in local scope may not leak.

  Furthermore, as a derived form, open declarations may contain arbitrary
  module expressions:

    fun sortWithoutDups compare =
        let
          open MkSet(type t = string; val compare = compare)
        in
          toList o foldr insert empty
        end

* First-class modules.

  Modules can be wrapped up as first-class values, by giving a module
  expression and an appropriate signature:

    val p = pack Int : INTEGER

  The type of such a value is

    val p : pack INTEGER

  To unwrap a package, another signature constraint is necessary, e.g.:

    fun four x =
        let
          structure I = unpack x : INTEGER
        in
          I.toString(I.fromString "4")
        end

  Or, using generalised "open":

    fun four x =
        let
          open unpack x : INTEGER
        in
          toString(fromString "4")
        end

  The type of four is inferred as "pack INTEGER -> string".

----------

Please see INSTALL.txt for brief instructions and the doc directory for
the full documentation. See LICENSE.txt for licensing information.

All HaMLet sources (c) 1999-2025 Andreas Rossberg.

https://mpi-sws.org/~rossberg/hamlet/#successor-ml
