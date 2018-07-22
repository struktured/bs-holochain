open Constants

module GetOptions = struct
  type t = {
    statusMask : System.Status.t [@bs.as "StatusMask"] [@bs.optional];
    getMask : System.GetMask.t [@bs.as "GetMask"] [@bs.optional];
    local : bool [@bs.as "Local"] [@bs.optional];
    bundle : bool [@bs.as "Bundle"] [@bs.optional];
  } [@@bs.deriving abstract]
  let default =
    t ()
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
  val remove : ?message:string -> t HashString.t -> t HashString.t

  (** [toJson t] converts the entry [t] to a json structure. *)
  val toJson : t -> Js.Json.t
end

module Make ( E : S0 ) : S with type t = E.t = struct
  include E
  external convertType : Js.Json.t -> t = "%identity"
  external getOpt : t HashString.t -> GetOptions.t -> Js.Json.t = "get"
    [@@bs.val]
  external get : t HashString.t -> Js.Json.t = ""
    [@@bs.val]
  external makeHash : entryType:string -> t -> t HashString.t = "" [@@bs.val]
  external commit : entryType:string -> t -> t HashString.t = "" [@@bs.val]
  external update :
    entryType:string -> t -> t HashString.t -> t HashString.t = "" [@@bs.val]
  external remove :
    t HashString.t -> message:string Js.Null.t -> t HashString.t = "" [@@bs.val]
  external toJson : t -> Js.Json.t = "%identity"

  let makeHash t =
    Native.debug ("bs-holochain", "makeHash", name, t);
    makeHash ~entryType:name t

  let convertType = convertType
  let get ?options hashString =
    let json = match options with
      | None ->
        get hashString
      | Some options ->
        getOpt hashString options in
    match Js.Json.decodeString json with
    | Some s ->
      (match (s :> string) = System.hashNotFound with
       | true ->
         Native.debug
           ("bs-holochain: get", hashString, " hashNotFound, returning none");
         None
       | false ->
         Some (convertType json)
      )
    | None ->
      match Js.Json.test json Js.Json.Null with
      | true ->
        None
      | false ->
        Some (convertType json)


  let commit = commit ~entryType:E.name
  let update entry oldHash =
    update ~entryType:E.name entry oldHash
  let remove ?message h =
    remove h ~message:(Js.Null.fromOption message)
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



