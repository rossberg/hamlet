(*
 * (c) Andreas Rossberg 2001-2007
 *
 * Line/column counting
 *)

functor LineAwareLexer(
	structure Lexer: LEXER
		  where type UserDeclarations.pos = int
	      (**)where type ('a,'b) UserDeclarations.token =
			     ('a,'b) LrParser.Token.token
	exception Error of (int * int) * string
) =
struct

    structure UserDeclarations =
    struct
	open Lexer.UserDeclarations
	type pos = Source.pos
    end

    exception Error' = Error
    exception Error of Source.region * string

    fun makeLexer yyinput =
	let
	    val lin  = ref 1
	    val col  = ref 0
	    val pos  = ref 0
	    val buf  = ref ""		(* current buffer *)
	    val off  = ref 0		(* offset to start of current buffer *)
	    val que  = Queue.mkQueue ()	(* buffer queue *)
	    val offE = ref 0		(* end offset *)

	    fun count(i, i', lin, col) =
		if i = i' then
		    (lin,col)
		else (case String.sub(!buf, i)
		    of #"\n" => count(i+1, i', lin+1, 0)
		     | #"\t" => count(i+1, i', lin, col+8 - col mod 8)
		     |  _    => count(i+1, i', lin, col+1)
		) handle Subscript => (* end of current buffer *)
		let
		    val n = String.size(!buf)
		    val (buf',off') = Queue.dequeue que
		in
		    buf := buf';
		    off := off';
		    count(0, i'-n, lin, col)
		end handle Dequeue => (* end of file token *)
		    ( buf := ""
		    ; off := !offE
		    ; (lin,col)
		    )

	    fun transform(pos1, pos2) =
		let
		    val n0 = !off
		    val pos1' as (l1,c1) = count(!pos-n0, pos1-n0, !lin, !col)
		    val n0 = !off
		    val pos2' as (l2,c2) = count(pos1-n0, pos2-n0, l1, c1)
		in
		    lin := l2;
		    col := c2;
		    pos := pos2;
		    (pos1',pos2')
		end

	    fun yyinput' n =
		let
		    val s = yyinput n
		in
		    Queue.enqueue(que, (s, !offE));
		    offE := !offE + String.size s;
		    s
		end

	    val lexer = Lexer.makeLexer yyinput'
	in
	    fn () =>
		let
		    val LrParser.Token.TOKEN(term, (svalue,pos1,pos2)) = lexer()
		    val (pos1', pos2') = transform(pos1, pos2)
		in
		    LrParser.Token.TOKEN(term, (svalue, pos1', pos2'))
		end
		handle Error'(position, e) => raise Error(transform position, e)
	end
end
