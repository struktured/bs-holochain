open Types

module type S0 = Named.S

module GetLinks = struct

  type packed =
    {hash:string;entryType:string;entry:Js.Json.t;source:[`Agent] hashString}

  let getLinks
      ?(tag:string option)
      ?(options:LinkOptions.t option) ~base =
    let entries = Native.getLinks ?tag ?options ~base in
    Array.map
      (fun entryInfo ->
         match Js.Json.decodeObject entryInfo with
         | None ->
           Js.log2 "unexpected link info shape:" entryInfo;
           failwith "unexpected link info shape"
         | Some dict ->
           let hash =
             Belt_Option.getExn (Js.Dict.get dict "Hash") |>
             Js.Json.stringify in
           (match Belt_Option.map
                    (Js.Dict.get dict "EntryType") Js.Json.stringify with
           | None -> `Hash hash
           | Some entryType ->
             let entry = Belt_Option.getExn
                 (Js.Dict.get dict "Entry") in
             let source =
               Belt_Option.getExn (Js.Dict.get dict "Source") |>
               Js.Json.stringify in
             `Packed {hash; entryType; entry; source}
           )
      )
      entries

  type 'entry unpacked =
    {entry:'entry;source:[`Agent] hashString;hash:'entry hashString}

  let unpack
      (type entry)
      (module E : Entry.S with type t = entry)
      links : entry unpacked array =
    Belt_Array.keepMap links
      (function
        | `Hash (_hash:string) -> None
        | `Packed {hash;entryType;entry;source} ->
          match entryType = E.name with
          | true -> Some {source;entry=E.convertType entry;
                          hash=E.hashOfString hash}
          | false -> None
      )

end


module Builder () =
struct

  module type HANDLER = sig
    include Entry.S
    include Validate.S with type t := t
  end


  let entries : (module HANDLER) Belt_Map.String.t ref
    = ref Belt_Map.String.empty

  module Add
      (E0 : Entry.S0)
      (V : Validate.S with type t = E0.t) :
    Entry.S with type t = E0.t = struct
    module E = Entry.Make(E0)
    module Handler : HANDLER with type t = E.t =
      struct
        include (E : Entry.S with type t = E.t)
        include (V : Validate.S with type t := t)
      end
    include (E : Entry.S with type t = E0.t)
    let () = entries :=
        Belt_Map.String.set
          (!entries) E0.name
          (module Handler : HANDLER)

  end

  module Build
      (G : Genesis.S)
      (SR : Sendreceive.S0) =
  struct
    include Sendreceive.Make(SR)

    let moduleOfEntryType (entryType:string) =
      Belt_Map.String.get (!entries) entryType

    let moduleOfEntryTypeExn entryType =
      match moduleOfEntryType entryType with
      | None -> failwith "no module for entry type"
      | Some m -> m

    module Callback : Callbacks.REQUIRED = struct
      include G

      let validateCommit ~entryType ~entry ~package ~sources =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        H.validateCommit
          ~package
          ~sources
          (H.convertType (entry : Js.Json.t))

      let validatePut ~entryType ~entry ~header ~package ~sources =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        H.validatePut
          ~header
          ~package
          ~sources
          (H.convertType entry)

      let validateMod ~entryType ~entry
          ~header ~(replaces:string) ~package ~sources =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        let replaces = H.hashOfString replaces in
        H.validateMod
          ~header
          ~replaces
          ~package
          ~sources
          (H.convertType entry)

      let validateDel ~entryType
          ~(hash:string) ~package ~sources =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        let hash = H.hashOfString hash in
        H.validateDel
          ~hash
          ~package
          ~sources

      let validateLink ~entryType
          ~(hash:string) ~links ~package ~sources =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        let hash = H.hashOfString hash in
        H.validateLink
          ~hash
          ~links
          ~package
          ~sources

      let validatePutPkg ~entryType =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        H.validatePutPkg ()

      let validateModPkg ~entryType =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        H.validateModPkg ()

      let validateDelPkg ~entryType =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        H.validateDelPkg ()

      let validateLinkPkg ~entryType =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        H.validateLinkPkg ()
    end
    include Callback
  end
end


