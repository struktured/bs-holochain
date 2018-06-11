(** Implements a send and receive coupling for some particular
    zome and input / utput types. 
*)

open Types

module type S0 =
sig
  type input (** the arguments to [send] *)
  type output (** the output returned by [receive] *)
  val receive : hashString -> input -> output
end

module type S = sig
  include S0
  val send : input -> output
end

module Make (T : S0) :
  S with type input = T.input with type output = T.output =
struct
  include T
  external send : input -> output = "" [@@bs.val]
  let send = send
end


