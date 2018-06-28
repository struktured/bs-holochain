(** Validation callback api, but only for a specific entry type *)
open Types
module type S =
  sig
  (** The entry type being validated *)
  type t
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
    replaces:t hashString ->
    package:Js.Json.t ->
    sources:string array ->
    t ->
    bool
  val validateDel :
    hash:t hashString ->
    package:Js.Json.t ->
    sources:string array ->
    bool
  val validateLink :
    hash:t hashString ->
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

