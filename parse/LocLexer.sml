(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Line/column counting
 *)

functor LocLexer(
  structure RawLexer: LEXER
    where type UserDeclarations.pos = int
      and type ('a,'b) UserDeclarations.token = ('a,'b) LrParser.Token.token
  exception RawError of (int * int) * string
  exception LocError of Source.region * string
) =
struct
  structure UserDeclarations =
  struct
    open RawLexer.UserDeclarations
    type pos = Source.pos
  end

  fun makeLexer yyinput =
      let
        val line = ref 1
        val col = ref 0
        val pos = ref 0
        val buf = ref ""                (* current buffer *)
        val off = ref 0                 (* offset to start of current buffer *)
        val que = Queue.mkQueue()       (* buffer queue *)
        val eos = ref 0                 (* end offset *)

        fun count(i, i', line, col) =
            if i = i' then
              (line, col)
            else if i < String.size(!buf) then
              case String.sub(!buf, i) of
                #"\n" => count(i + 1, i', line + 1, 0)
              | #"\t" => count(i + 1, i', line, col + 8 - col mod 8)
              |  _    => count(i + 1, i', line, col + 1)
            else if Queue.isEmpty que then
              ( buf := ""; off := !eos; (line, col) )
            else
              let
                val n = String.size(!buf)
                val (buf', off') = Queue.dequeue que
              in
                buf := buf';
                off := off';
                count(0, i' - n, line, col)
              end

        fun region(pos1, pos2) =
            let
              val n0 = !off
              val pos1' as (l1, c1) = count(!pos - n0, pos1 - n0, !line, !col)
              val n0 = !off
              val pos2' as (l2, c2) = count(pos1 - n0, pos2 - n0, l1, c1)
            in
              line := l2;
              col := c2;
              pos := pos2;
              (pos1', pos2')
            end

        fun yyinput' n =
            let
              val s = yyinput n
            in
              Queue.enqueue(que, (s, !eos));
              eos := !eos + String.size s;
              s
            end

        val lexer = RawLexer.makeLexer yyinput'
      in
        fn() =>
          let
            val LrParser.Token.TOKEN(term, (svalue, pos1, pos2)) = lexer()
            val (pos1', pos2') = region(pos1, pos2)
          in
            LrParser.Token.TOKEN(term, (svalue, pos1', pos2'))
          end
          handle RawError(range, e) => raise LocError(region range, e)
      end
end;
