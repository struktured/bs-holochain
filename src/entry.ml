open Types
module type S0 =
sig
  include Named.S
  type t [@@bs.deriving abstract]
end

module type S = sig
  include S0
  val convertType : Js.Json.t -> t
  val get : hashString -> options:Js.Json.t -> t
  val commit : t -> hashString
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


let get (type t) (module E : S0 with type t = t) =
  let module Entry = Make(E) in
  Entry.get

let makeHash (type t) (module E : S0 with type t = t) =
  let module Entry = Make(E) in
  Entry.makeHash

let commit (type t) (module E : S0 with type t = t) =
  let module Entry = Make(E) in
  Entry.commit



