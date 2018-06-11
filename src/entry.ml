open Types
module type S0 =
sig
  include Named.S
  type t [@@bs.deriving abstract]

  val validateCommit :
    package:Js.Json.t ->
    sources: string array ->
    t ->
    bool
  val validatePut :
    header:Js.Json.t ->
    package:Js.Json.t ->
    sources:string array ->
    t ->
    bool
  val validateMod :
    header:Js.Json.t ->
    replaces:hashString ->
    package:Js.Json.t ->
    sources:string array ->
    t ->
    bool
  val validateDel :
    hash:hashString ->
    package:Js.Json.t ->
    sources:string array ->
    bool
  val validateLink :
    hash:hashString ->
    package:Js.Json.t ->
    sources:string array ->
    links:Js.Json.t array ->
    bool

  val validatePutPkg :
    unit -> Js.Json.t
  val validateModPkg :
    unit -> Js.Json.t
  val validateDelPkg :
    unit -> Js.Json.t
  val validateLinkPkg :
    unit -> Js.Json.t
end

module type S = sig
  include S0
  val convertType : Js.Json.t -> t
  val get : hashString -> options:Js.Json.t -> t
  val makeHash : t -> hashString
end

module Make ( E : S0 ) : S with type t = E.t = struct
  include E
  external convertType : Js.Json.t -> t = "%identity"
  external get : hashString -> options:Js.Json.t -> t = "" [@@bs.val]
  external makeHash : entryType:string -> t -> hashString = "" [@@bs.val]
  external commit : entryType:string -> t -> hashString = "" [@@bs.val]
  let makeHash = makeHash ~entryType:name
  let convertType = convertType
  let get = get
  let commit = commit ~entryType:E.name
end


