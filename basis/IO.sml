(*
 * (c) Andreas Rossberg 2001-2013
 *
 * Standard ML Basis Library
 *)

structure IO :> IO =
struct
  exception Io = IO.Io  (* primitive *)

  exception BlockingNotSupported
  exception NonblockingNotSupported
  exception RandomAccessNotSupported
  exception ClosedStream

  datatype buffer_mode = NO_BUF | LINE_BUF | BLOCK_BUF
end;
