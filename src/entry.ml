open Types


(**
   {1} Entry definitions and functors.
*)

module type S0 =
sig
  include Named.S
  type t
end

module type S = sig
  include S0
  val convertType : Js.Json.t -> t
  val get : ?options:GetOptions.t -> t hashString -> t option
  val commit : t -> t hashString
  val makeHash : t -> t hashString
  val hashOfString : string -> t hashString
end

module Make ( E : S0 ) : S with type t = E.t = struct
  include E
  external convertType : Js.Json.t -> t = "%identity"
  external get : t hashString -> options:GetOptions.t option -> t option = ""
    [@@bs.val] [@@bs.return nullable]
  external makeHash : entryType:string -> t -> t hashString = "" [@@bs.val]
  external commit : entryType:string -> t -> t hashString = "" [@@bs.val]
  let makeHash = makeHash ~entryType:name
  let convertType = convertType
  let get ?options hashString = get hashString ~options
  let commit = commit ~entryType:E.name

  let hashOfString (s:string) : t hashString =
    HashString.create s
end


(** {1} Functorless entry functions, all which require
    first class entry modules of type [S0].
*)

let get (type t) (module E : S0 with type t = t) =
  let module Entry = Make(E) in
  Entry.get

let makeHash (type t) (module E : S0 with type t = t) =
  let module Entry = Make(E) in
  Entry.makeHash

let commit (type t) (module E : S0 with type t = t) =
  let module Entry = Make(E) in
  Entry.commit

let hashOfString (type t) (module E : S0 with type t = t) =
  let module Entry = Make(E) in
  Entry.hashOfString



