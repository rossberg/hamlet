(* CharVector.all is missing in Moscow ML *)

structure CharVector =
struct
  open CharVector

  local
    fun all'(p, s, i) =
        i = String.size s orelse p(String.sub(s, i)) andalso all'(p, s, i + 1)
  in
    fun all p s = all'(p, s, 0)
  end
end
