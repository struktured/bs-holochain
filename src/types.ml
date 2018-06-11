type hashString = string

(** Represents a link from an entry at [base] to hash [link]. [tag] is optional
    metadata for filtering and finding links.
*)
type link = {
  base : hashString [@bs.as "Base"];
  link : hashString [@bs.as "Link"];
  tag : string [@bs.as "Tag"];
} [@@bs.deriving abstract]

(** Options for the getLinks function. If [load] is true
    a list of entrys is returned.

    If [load] is false (the default value),
    only the hash of the entries are provided in the list.

    [statusMask] allows filtering of entries according to their
    status. One of [`Live | `Deleted | `Rejected].
*)
type linkOptions = {
  load : bool [@bs.as "Load"];
  statusMask :
    [`Live | `Deleted | `Reject] [@bs.as "StatusMask"] [@bs.int]
} [@@bs.deriving abstract]

(** The type of a bridge. If side is [`From] then [toApp] is non empty. If side
    is [`To], [token] is non empty. *)
type bridge =
{
  toApp : hashString;
  side:([`From | `To] [@bs.int]);
  token:string
} [@@deriving bs.abstract]


