open Types
(** A zome funtion with strict input and output type definitions. *)
module type S0 =
sig
  include Named.S
  type input
  type output
  (** Invoke the function on the local chain directly *)
  val local : input -> output
end

module type S = sig
  include S0
  val call : input -> output
end

module Make (Z : Named.S) (T : S0) :
  S with type input = T.input with type output = T.output = struct
  include T
  external call :
    zomeName:string -> functionName:string -> input -> output = "" [@@bs.val]
  let call args = call ~zomeName:Z.name ~functionName:T.name args
end


