(** {2} Holochain system constants *)
open Types

module HC = struct
  external version :
    string = "Version" [@@bs.module "HC"] [@@bs.val]

  external hash_not_found :
    hash_string = "HashNotFound" [@@bs.module "HC"] [@@bs.val]

  type status =
    [`Live | `Deleted | `Modified | `Rejected | `Any]
  [@@bs.module "HC"] [@@bs.enum]

  type get_mask =
    [`Default | `Entry | `EntryType | `Sources | `All]
  [@@bs.module "HC"]  [@@bs.enum]


  type link_action = [`Add | `Del] [@@bs.module "HC"] [@@bs.enum]

  type pkg_req =
    [`Chain | `ChainOpt | `EntryTypes] [@@bs.module "HC"] [@@bs.enum]

  type chain_opt =
    [`Chain | `ChainOpt | `EntryTypes] [@@bs.module "HC.PkgReq"] [@@bs.enum]

  type bridge = [`From | `To] [@@bs.module "HC"] [@@bs.enum]

  type sys_entry_type = [`DNA|`Agent|`Key|`Headers|`Del]
  [@@bs.module "HC"] [@@bs.enum]
end

(** Holochain application constants *)
module App = struct

  (** Holds the Name of this Holochain from the DNA. *)
  external name : string = "name" [@@bs.module "App"] [@@bs.val]

  (** Holochain's DNA related constants *)
  module DNA = struct
    (** Holds the unique identifier of this Holochain's DNA. Nodes must run the same DNA to be on the same Holochain. *)
    external hash : string = "Hash" [@@bs.module "App.DNA"] [@@bs.val]
  end

  (** Holochain's Agent related constants *)
  module Agent = struct
    (** Holds your peer's identity info on the DHT. This is the hash for the second entry (identity info) on your chain. **)
    external hash : string = "Hash" [@@bs.module "App.Agent"] [@@bs.val]
    (** Holds the most recent agent indentity entry that has been committed to the chain. To start with its value is equivalent to App.Agent.Hash after a call to updateAgent it will have the value of the newly committed agent entry. *)
    external top_hash : string = "TopHash"[@@bs.module "App.Agent"] [@@bs.val]

    (** Holds the identity string used to initialize the holochain software with hcadmin init If you used JSON to embed multiple properties (such as FirstName, LastName, Email, etc), they can be retrieved here as App.Agent.FirstName, etc. *)
    external string : string = "String" [@@bs.module "App.Agent"] [@@bs.val]

  end

  (** Holochain's Key related constants *)
  module Key = struct
    (* Holds the hash of your public key. This is your node address on the DHT. It can be used for node-to-node messaging with send and receive functions. *)
    external hash : string = "Hash" [@@bs.module "App.DNA"] [@@bs.val]
  end

end


