open Types

(** Module which requires a string name identifier *)
module Named = struct
  module type S = sig val name : string end
end


module Zome0 =
struct
  module type S0 =
  sig
    include Named.S
  end
end

module SendReceive =
struct
  module type S0 =
  sig
    type input
    type output
    val receive : hashString -> input -> output
  end

  module type S = sig
    include S0
    val send : input -> output
  end

  module Make (T : S0) :
    S with type input = T.input with type output = T.output =
  struct
    include T
    external send : input -> output = "" [@@bs.val]
    let send = send
  end
end

(** A zome funtion with strict input and output type definitions. *)
module Function =
struct
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

  module Make (Z : Zome0.S0) (T : S0) :
    S with type input = T.input with type output = T.output = struct
    include T
    external call :
      zomeName:string -> functionName:string -> input -> output = "" [@@bs.val]
    let call args = call ~zomeName:Z.name ~functionName:T.name args
  end
end

module Entry =
struct
  module type S0 =
  sig
    val name : string
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
end


module GetLinks = struct

  type packed =
    {hash:hashString;entryType:string;entry:Js.Json.t;source:hashString}

  let getLinks ?(tag:string option) ?(options:linkOptions option) ~base =
    let entries = Raw.getLinks ?tag ?options ~base in
    Array.map
      (fun entry_info ->
         match Js.Json.decodeObject entry_info with
         | None ->
           Js.log2 "unexpected link info shape:" entry_info;
           failwith "unexpected link info shape"
         | Some dict ->
           let hash:hashString =
             Belt_Option.getExn (Js.Dict.get dict "Hash") |>
             Js.Json.stringify in
           (match Belt_Option.map
                    (Js.Dict.get dict "EntryType") Js.Json.stringify with
           | None -> `Hash hash
           | Some entryType ->
             let entry = Belt_Option.getExn
                 (Js.Dict.get dict "Entry") in
             let source : hashString =
               Belt_Option.getExn (Js.Dict.get dict "Source") |>
               Js.Json.stringify in
             `Entry {hash;entryType; entry; source}
           )
      )
      entries

  type 'a unpacked = {entry:'a;source:hashString;hash:hashString}

  let unpack
      (type entry)
      (module E : Entry.S with type t = entry)
      links : entry unpacked array =
    Belt_Array.keepMap links
      (function
        | `Hash (_hash:hashString) -> None
        | `Entry {hash;entryType;entry;source} ->
          match entryType = E.name with
          | true -> Some {source;entry=E.convertType entry;hash}
          | false -> None
      )

end

module Zome =
struct

  module Builder () =
  struct

    let entries : (module Entry.S) list ref = ref []

    module Add(E0 : Entry.S0) : Entry.S with type t = E0.t = struct
      module E = Entry.Make(E0)
      include (E : Entry.S with type t = E0.t)
      let () = entries := (module E : Entry.S) :: !entries
    end

    module Build
        (G : sig val genesis : unit -> bool end)
        (SR : SendReceive.S0) =
    struct
      include SendReceive.Make(SR)

      let moduleOfEntryType (entryType:string) =
        Belt_List.keep (!entries)
          (fun (module E:Entry.S) -> entryType = E.name) |>
        Belt_List.head

      let moduleOfEntryType_exn entryType =
        match moduleOfEntryType entryType with
        | None -> failwith "no module for entry type"
        | Some m -> m

      module Callback : Callbacks.REQUIRED = struct
        include G

        let validateCommit ~entryType ~entry ~package ~sources =
          let m = moduleOfEntryType_exn entryType in
          let module E = (val m : Entry.S) in
          E.validateCommit
            ~package
            ~sources
            (E.convertType (entry : Js.Json.t))

        let validatePut ~entryType ~entry ~header ~package ~sources =
          let m =  moduleOfEntryType_exn entryType in
          let module E = (val m : Entry.S) in
          E.validatePut
            ~header
            ~package
            ~sources
            (E.convertType entry)

        let validateMod ~entryType ~entry
            ~header ~replaces ~package ~sources =
          let m = moduleOfEntryType_exn entryType in
          let module E = (val m : Entry.S) in
          E.validateMod
            ~header
            ~replaces
            ~package
            ~sources
            (E.convertType entry)

        let validateDel ~entryType
            ~hash ~package ~sources =
          let m = moduleOfEntryType_exn entryType in
          let module E = (val m : Entry.S) in
          E.validateDel
            ~hash
            ~package
            ~sources

        let validateLink ~entryType
            ~hash ~links ~package ~sources =
          let m = moduleOfEntryType_exn entryType in
          let module E = (val m : Entry.S) in
          E.validateLink
            ~hash
            ~links
            ~package
            ~sources

        let validatePutPkg ~entryType =
          let m = moduleOfEntryType_exn entryType in
          let module E = (val m : Entry.S) in
          E.validatePutPkg ()

        let validateModPkg ~entryType =
          let m = moduleOfEntryType_exn entryType in
          let module E = (val m : Entry.S) in
          E.validateModPkg ()

        let validateDelPkg ~entryType =
          let m = moduleOfEntryType_exn entryType in
          let module E = (val m : Entry.S) in
          E.validateDelPkg ()

        let validateLinkPkg ~entryType =
          let m = moduleOfEntryType_exn entryType in
          let module E = (val m : Entry.S) in
          E.validateLinkPkg ()
      end
      include Callback
    end
  end

end

