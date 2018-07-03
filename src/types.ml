
(** Module which requires a string name identifier *)
module Named = struct
  module type S = sig val name : string end
end

type statusMask =
  [`Live | `Deleted | `Reject] [@bs.int]

(** TODO decide module scoping *)
type getMask =
  [`Entry | `EntryType | `Sources | `All]
    [@bs.int] [@bs.module "HC"]

module HashString : sig
  type 'entry t = private string
  (** Manually lift a string to a hash string type. Only should be used as a last resort.
      Insteaad, use the entry module level version or those in the constants module
   *)
  val create : string -> 'entry t
  val equals : 'entry t -> 'entry t -> bool
  val hashEquals : 'entry t -> 'entry' t -> bool
   end =
struct
  type 'entry t = string
  let create s : 'entry t = s
  let hashEquals = (=)
  let equals = hashEquals
end

type 'entry hashString = 'entry HashString.t

(** Represents a link from an entry at [base] to hash [link]. [tag] is optional
    metadata for filtering and finding links.
*)
module Link = struct
  type ('base, 'to_) t = {
    base : 'base hashString [@bs.as "Base"];
    to_ : 'to_ hashString [@bs.as "Link"];
    tag : string option [@bs.as "Tag"]; (* TODO make polymorphic abstraction ? *)
  } [@@bs.deriving abstract]
end

(** Options for the getLinks function. If [load] is true
    a list of entrys is returned.

    If [load] is false (the default value),
    only the hash of the entries are provided in the list.

    [statusMask] allows filtering of entries according to their
    status. One of [`Live | `Deleted | `Rejected].
*)
module LinkOptions =
struct
  type t = {
    load : bool [@bs.as "Load"];
    statusMask : statusMask
  } [@@bs.deriving abstract]

  let defaultLinkOptions =
    t ~load:false ~statusMask:`Live
end

module Bridge =
struct
  (** The type of a bridge. If side is [`From] then [toApp] is non empty. If side
      is [`To], [token] is non empty. *)
  type 'entry t =
    {
      toApp : 'entry hashString;
      side:([`From | `To] [@bs.int]);
      token:string
    } [@@deriving bs.abstract]
end

module GetOptions = struct
  type t = {
    statusMask : statusMask [@bs.as "StatusMask"];
    getMask : getMask [@bs.as "GetMask"];
    local : bool [@bs.as "Local"];
    bundle : bool [@bs.as "Bundle"];
  } [@@deriving bs.abstract]
end
