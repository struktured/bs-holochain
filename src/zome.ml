open Printf
(**
 * A minimally defined Zome is just a named entity.
*)
module type S0 = Named.S


(** A validation handler for a particular entry module *)
module type HANDLER =
sig
    include Entry.S
    include Validate.S with type t := t
end

(** Builder to produce a zome. *)
module Builder (Z:S0) =
struct

  let entries : (module HANDLER) Belt_Map.String.t ref
    = ref Belt_Map.String.empty

  module Handler(Handler:HANDLER) =
  struct
    include (Handler : Entry.S with type t = Handler.t)
    let () = entries :=
        Belt_Map.String.set
          (!entries) Handler.name
          (module Handler : HANDLER)
  end

  module EntryBs
      (E: Entry.E_BS)
      (V : Validate.S with type t = E.t) :
    Entry.S with type t = E.t = struct
    module E = Entry.Make_bs(E)
    module H : HANDLER with type t = E.t =
    struct
      include (E : Entry.S with type t = E.t)
      include (V : Validate.S with type t := t)
    end
    include Handler(H)
  end


  module Entry0
      (E0 : Entry.S0)
      (V : Validate.S with type t = E0.t) :
    Entry.S with type t = E0.t = struct
    module E = Entry.Make(E0)
    module H : HANDLER with type t = E.t =
    struct
      include (E : Entry.S with type t = E.t)
      include (V : Validate.S with type t := t)
    end
    include Handler(H)
  end

  module Entry
      (E : Entry.S)
      (V : Validate.S with type t = E.t) :
    Entry.S with type t = E.t = struct
    module H : HANDLER with type t = E.t =
    struct
      include (E : Entry.S with type t = E.t)
      include (V : Validate.S with type t := t)
    end
    include Handler(H)
  end

  module type S = sig
    include S0
    include Sendreceive.S0
    include Callbacks.REQUIRED
  end

  module Build
      (G : Genesis.S)
      (SR : Sendreceive.S0) :
    S with type input = SR.input and type output = SR.output =
  struct
    include Z
    include SR

    let moduleOfEntryType (entryType:string) =
      Belt_Map.String.get (!entries) entryType

    let moduleOfEntryTypeExn entryType =
      match moduleOfEntryType entryType with
      | None ->
        failwith
          (sprintf "no module for entry type: %s" entryType)
      | Some m -> m

    module Callback : Callbacks.REQUIRED = struct
      include G

      let validateCommit ~entryType ~entry ~header ~package ~sources =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        H.validateCommit
          ~header
          ~package
          ~sources
          (H.ofJson (entry : Js.Json.t))

      let validatePut ~entryType ~entry ~header ~package ~sources =
        let m = moduleOfEntryTypeExn entryType in
        let module H = (val m : HANDLER) in
        H.validatePut
          ~header
          ~package
          ~sources
          (H.ofJson entry)

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
          (H.ofJson entry)

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


