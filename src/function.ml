(** A zome function with strict input and output type definitions.
    It must be a exported function equal to the [name] field or you
    will experience runtime errors.

    Note that at the moment this module is only useful to dependent
    client libraries since functions can be invoked locally otherwise.
*)

module type S0 =
sig
  module Zome : Named.S
  include Named.S
  type input
  type output
end

module type S = sig
  include Named.S
  type input
  type output
  val call : input -> output
end

module Make (T : S0) :
  S with type input = T.input with type output = T.output = struct
  include T
  external call :
    zomeName:string -> functionName:string -> input -> output = "" [@@bs.val]
  let call args = call ~zomeName:T.Zome.name ~functionName:T.name args
end

(** [call (module T) args] calls function [T] with [args] in the zome as
    described by [T.Zome].
*)
let call
    (type input) (type output)
    (module T:S0 with type input = input and type output = output) =
  let module M = Make(T) in
  M.call
