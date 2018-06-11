open Types

module type S0 = Named.S

module GetLinks = struct

  type packed =
    {hash:hashString;entryType:string;entry:Js.Json.t;source:hashString}

  let getLinks ?(tag:string option) ?(options:linkOptions option) ~base =
    let entries = Raw.getLinks ?tag ?options ~base in
    Array.map
      (fun entryInfo ->
         match Js.Json.decodeObject entryInfo with
         | None ->
           Js.log2 "unexpected link info shape:" entryInfo;
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
      (SR : Sendreceive.S0) =
  struct
    include Sendreceive.Make(SR)

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


