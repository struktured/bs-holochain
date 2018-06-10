open Types

module Zome0 =
struct
  module type S0 =
  sig
    val name : string
  end
end

module SendReceive =
struct
  module type S0 =
  sig
    type input
    type output
    val receive : hash_string -> input -> output
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

module Function =
struct
  module type S0 =
  sig
    type input
    type output
    val name : string
  end

  module type S = sig
    include S0
    val call : input -> output
  end

  module Make (Z : Zome0.S0) (T : S0) :
    S with type input = T.input with type output = T.output = struct
    include T
    external call : zome_name:string -> function_name:string -> input -> output = "" [@@bs.val]
    let call args = call ~zome_name:Z.name ~function_name:T.name args
  end
end

module Entry =
struct
  module type S0 =
  sig
    val name : string
    type t [@@bs.deriving abstract]

    val validate_commit :
      package:Js.Json.t ->
      sources: string array ->
      t ->
      bool
    val validate_put :
      header:Js.Json.t ->
      package:Js.Json.t ->
      sources:string array ->
      t ->
      bool
    val validate_mod :
      header:Js.Json.t ->
      replaces:hash_string ->
      package:Js.Json.t ->
      sources:string array ->
      t ->
      bool
    val validate_del :
      hash:hash_string ->
      package:Js.Json.t ->
      sources:string array ->
      bool
    val validate_link :
      hash:hash_string ->
      package:Js.Json.t ->
      sources:string array ->
      links:Js.Json.t array ->
      bool

    val validate_put_pkg :
      unit -> Js.Json.t
    val validate_mod_pkg :
      unit -> Js.Json.t
    val validate_del_pkg :
      unit -> Js.Json.t
    val validate_link_pkg :
      unit -> Js.Json.t
  end

  module type S = sig
    include S0
    val convert_type : Js.Json.t -> t
    val get : hash_string -> options:Js.Json.t -> t
    val make_hash : t -> hash_string
  end

  module Make ( E : S0 ) : S with type t = E.t = struct
    include E
    external convert_type : Js.Json.t -> t = "%identity"
    external get : hash_string -> options:Js.Json.t -> t = "" [@@bs.val]
    external make_hash : entry_type:string -> t -> hash_string = "makeHash" [@@bs.val]
    external commit : entry_type:string -> t -> hash_string = "" [@@bs.val]
    let make_hash = make_hash ~entry_type:name
    let convert_type = convert_type
    let get = get
    let commit = commit ~entry_type:E.name
  end
end

module Zome =
struct

  module Builder() =
  struct

    let entries : (module Entry.S) list ref = ref []

    module Add(E0 : Entry.S0) : Entry.S = struct
      module E = Entry.Make(E0)
      include (E : Entry.S)
      let () = entries := (module E : Entry.S) :: !entries
    end

    module Build
        (G : sig val genesis : unit -> bool end)
        (SR : SendReceive.S0) =
    struct
      include SendReceive.Make(SR)

      let module_of_entry_type (entry_type:string) =
        Belt_List.keep (!entries)
          (fun (module E:Entry.S) -> entry_type = E.name) |>
        Belt_List.head

      let module_of_entry_type_exn entry_type =
        match module_of_entry_type entry_type with
        | None -> failwith "no module for entry type"
        | Some m -> m

      module Callback : Callbacks.REQUIRED = struct
        include G

        let validateCommit ~entry_type ~entry ~package ~sources =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_commit
            ~package
            ~sources
            (E.convert_type (entry : Js.Json.t))

        let validatePut ~entry_type ~entry ~header ~package ~sources =
          let m =  module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_put
            ~header
            ~package
            ~sources
            (E.convert_type entry)

        let validateMod ~entry_type ~entry
            ~header ~replaces ~package ~sources =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_mod
            ~header
            ~replaces
            ~package
            ~sources
            (E.convert_type entry)

        let validateDel ~(entry_type:string)
            ~hash ~package ~sources =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_del
            ~hash
            ~package
            ~sources

        let validateLink ~entry_type
            ~hash ~links ~package ~sources =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_link
            ~hash
            ~links
            ~package
            ~sources

        let validatePutPkg ~entry_type =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_put_pkg ()

        let validateModPkg ~entry_type =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_mod_pkg ()

        let validateDelPkg ~entry_type =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_del_pkg ()

        let validateLinkPkg ~entry_type =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_link_pkg ()
      end
      include Callback
    end
  end

end

