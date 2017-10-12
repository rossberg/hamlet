(*
 * (c) Andreas Rossberg 1999-2013
 *
 * Standard ML abstract program syntax
 *
 * Definition, Section 8
 *)

functor SyntaxProgramFn(
  structure Module :
  sig
    type TopDec
  end

  type Program_attr
) =
struct
  (* Import *)

  open Module

  type ('a, 'b) phrase = ('a, 'b) Annotation.phrase


  (* Programs *)

  datatype Program' =
      Program of TopDec * Program option


  (* Annotated syntax *)

  withtype Program = (Program', Program_attr) phrase
end;
