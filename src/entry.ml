open Constants

module GetOptions = struct
  type t = {
    statusMask : System.Status.t [@bs.as "StatusMask"];
    getMask : System.GetMask.t [@bs.as "GetMask"];
    local : bool [@bs.as "Local"];
    bundle : bool [@bs.as "Bundle"];
  } [@@bs.deriving abstract]
end
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
  val get : ?options:GetOptions.t -> t HashString.t -> t option
  val commit : t -> t HashString.t
  val makeHash : t -> t HashString.t
  val hashOfString : string -> t HashString.t
  val update : t -> t HashString.t -> t HashString.t
  (** [toJson t] converts the entry [t] to a json structure. *)
  val toJson : t -> Js.Json.t
end

module Make ( E : S0 ) : S with type t = E.t = struct
  include E
  external convertType : Js.Json.t -> t = "%identity"
  external get : t HashString.t -> options:GetOptions.t option -> t option = ""
    [@@bs.val] [@@bs.return nullable]
  external makeHash : entryType:string -> t -> t HashString.t = "" [@@bs.val]
  external commit : entryType:string -> t -> t HashString.t = "" [@@bs.val]
  external update :
    entryType:string -> t -> t HashString.t -> t HashString.t = "" [@@bs.val]
  external toJson : t -> Js.Json.t = "%identity"

  let makeHash = makeHash ~entryType:name
  let convertType = convertType
  let get ?options hashString = get hashString ~options
  let commit = commit ~entryType:E.name
  let update entry oldHash =
    update ~entryType:E.name entry oldHash

  let hashOfString (s:string) : t HashString.t =
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



