type hash_string = string


(** Represents a link from an entry at [base] to hash [link]. [tag] is optional
    metadata for filtering and finding links.
*)
type link = {
  base : hash_string [@bs.as "Base"];
  link : hash_string [@bs.as "Link"];
  tag : string [@bs.as "Tag"];
} [@@bs.deriving abstract]

let x = Raw.get_links

(** Options for the getLinks function. If [load] is true
    a list of entrys is returned.

    If [load] is false (the default value),
    only the hash of the entries are provided in the list.

    [statusMask] allows filtering of entries according to their
    status. One of [`Live | `Deleted | `Rejected].
*)
type link_options = {
  load : bool [@bs.as "Load"];
  statusMask :
    [`Live | `Deleted | `Reject] [@bs.as "StatusMask"] [@bs.int]
} [@@bs.deriving abstract]
