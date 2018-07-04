open Constants
(** Implements a send and receive coupling for some particular
    zome and input / output types.
*)

open Types

(** Base signature for send and receive functionality *)
module type S0 =
sig
  type input (** the arguments to [send] *)
  type output (** the output returned by [receive] *)

  (** Callback to receive a message via the send / receive protocol.
      The [hashString] is the sender address. The callback must
      implement a value from type [output] derived from type [input].

      It may use other native holochain functions to accomplish, including
      mutating operations.
  *)
  val receive : App.Agent.hash -> input -> output
end

module type S = sig
  include S0
  val send : App.Agent.hash -> input -> output
end

module Make (T : S0) :
  S with type input = T.input with type output = T.output =
struct
  include T
  external send : App.Agent.hash -> input -> output = "" [@@bs.val]
  let send = send
end


