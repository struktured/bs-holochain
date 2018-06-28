(** {2} Holochain system constants *)
open Types

module System = struct
  external version :
    string = "Version" [@@bs.module "HC"] [@@bs.val]
  let version = version
  external hashNotFound :
    [`Hash_not_found] hashString = "HashNotFound" [@@bs.module "HC"] [@@bs.val]

  let hashNotFound = hashNotFound
  type status =
    [`Live | `Deleted | `Modified | `Rejected | `Any]
  [@@bs.module "HC"] [@@bs.enum]

  type getMask =
    [`Default | `Entry | `EntryType | `Sources | `All]
  [@@bs.module "HC"]  [@@bs.enum]

  type linkAction = [`Add | `Del] [@@bs.module "HC"] [@@bs.enum]

  type pkgReq =
    [`Chain | `ChainOpt | `EntryTypes] [@@bs.module "HC"] [@@bs.enum]

  type chainOpt =
    [`Chain | `ChainOpt | `EntryTypes] [@@bs.module "HC.PkgReq"] [@@bs.enum]

  type bridge = [`From | `To] [@@bs.module "HC"] [@@bs.enum]

  type sysEntryType =
    [`DNA|`Agent|`Key|`Headers|`Del] [@@bs.module "HC"] [@@bs.enum]
end

(** Holochain application constants *)
module App = struct

  (** Holds the Name of this Holochain from the DNA. *)
  external name : string = "name" [@@bs.module "App"] [@@bs.val]
  let name = name

  (** Holochain's DNA related constants *)
  module DNA = struct
    (** Holds the unique identifier of this Holochain's DNA.
       Nodes must run the same DNA to be on the same Holochain.
     * *)
    external hash : [`DNA] hashString =
      "Hash" [@@bs.module "App.DNA"] [@@bs.val]
    let hash = hash
  end

  (** Holochain's Agent related constants *)
  module Agent = struct

    external hash : [`Agent] hashString =
      "Hash" [@@bs.module "App.Agent"] [@@bs.val]

    (** Holds your peer's identity info on the DHT. This is the hash for the
     * second entry (identity info) on your chain. **)
    let hash = hash

    external topHash : [`Top] = "TopHash" [@@bs.module "App.Agent"] [@@bs.val]

    (** Holds the most recent agent indentity entry that has been committed to
     * the chain. To start with its value is equivalent to App.Agent.Hash after
     * a call to updateAgent it will have the value of the newly committed
     * agent entry. *)
    let topHash = topHash

    external string : string = "String" [@@bs.module "App.Agent"] [@@bs.val]

    (** Holds the identity string used to initialize the holochain software
     * with hcadmin init If you used JSON to embed multiple properties (such as
     * FirstName, LastName, Email, etc), they can be retrieved here as
     * App.Agent.FirstName, etc. *)
     let string = string

  end

  (** Holochain's Key related constants *)
  module Key = struct
   external hash : string = "Hash" [@@bs.module "App.DNA"] [@@bs.val]

   (* Holds the hash of your public key. This is your node address on the DHT.
    * It can be used for node-to-node messaging with send and receive
    * functions. *)
   let hash = hash
 end

end


