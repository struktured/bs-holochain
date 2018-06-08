open Types
module Entry =
struct
  module type S0 =
  sig
    val name : string
    type obj
    type t = obj Js.t

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
   module type S = sig include S0
     (*val get : hash_string -> options:Js.Json.t ->
       t option*)
   end
end

module Zome =
struct

  module Builder() =
  struct

    let entries : (module Entry.S) list ref = ref []

    module Add(E : Entry.S0) : Entry.S = struct
      let () = entries := (module E : Entry.S)::!entries
      include (E : Entry.S)
    end

    module Build(Genesis:sig val genesis : unit -> bool end) = struct

      let module_of_entry_type (entry_type:string) =
        Belt_List.keep (!entries)
          (fun (module E:Entry.S) -> entry_type = E.name) |>
        Belt_List.head

      let module_of_entry_type_exn entry_type =
        match module_of_entry_type entry_type with
        | None -> failwith "no module for entry type"
        | Some m -> m

      module Callback : Callbacks.REQUIRED = struct
        include Genesis

        let convert_type = Obj.magic
        let validateCommit ~entry_type ~entry ~package ~sources =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_commit
            ~package
            ~sources
            (convert_type (entry:Js.Json.t))

        let validatePut ~entry_type ~entry ~header ~package ~sources =
          let m =  module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_put
            ~header
            ~package
            ~sources
            (convert_type entry)

        let validateMod ~entry_type ~entry
            ~header ~replaces ~package ~sources =
          let m = module_of_entry_type_exn entry_type in
          let module E = (val m : Entry.S) in
          E.validate_mod
            ~header
            ~replaces
            ~package
            ~sources
            (convert_type entry)

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
    end
  end

end

