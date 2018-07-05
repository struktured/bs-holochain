(** Holochain global constants *)

(** {2} Holochain system constants *)
module System = struct
  external version :
    string = "Version" [@@bs.module "HC"] [@@bs.val]
  let version = version

  external hashNotFound : string =
    "HashNotFound" [@@bs.module "HC"] [@@bs.val]

  type hashNotFound = [`Hash_not_found] HashString.t

  let hashNotFound : hashNotFound =
    HashString.create hashNotFound

  module Status :
  sig
    type t
    val live : t
    val deleted : t
    val modified : t
    val rejected : t
    val any : t
  end =
  struct
    type t = int
    external live : t = "Live" [@@bs.module "HC.Status"] [@@bs.val]
    external deleted : t = "Deleted" [@@bs.module "HC.Status"] [@@bs.val]
    external modified : t = "Modified" [@@bs.module "HC.Status"] [@@bs.val]
    external rejected : t = "Rejected" [@@bs.module "HC.Status"] [@@bs.val]
    external any : t = "Any" [@@bs.module "HC.Status"] [@@bs.val]
  end

  module GetMask :
  sig
    type t
    val default : t
    val entry : t
    val entryType : t
    val sources : t
    val all : t
  end =
  struct
    type t = int

    external default : t = "Default" [@@bs.module "HC.GetMask"] [@@bs.val]
    external entry : t = "Entry" [@@bs.module "HC.GetMask"] [@@bs.val]
    external entryType : t = "EntryType" [@@bs.module "HC.GetMask"] [@@bs.val]
    external sources : t = "Sources" [@@bs.module "HC.GetMask"] [@@bs.val]
    external all : t = "All" [@@bs.module "HC.GetMask"] [@@bs.val]
  end

  module LinkAction :
  sig
    type t
    val add : t
    val del : t
  end
  = struct
      type t = int
      external add : t = "Add" [@@bs.module "HC.LinkAction"] [@@bs.val]
      external del : t = "Del" [@@bs.module "HC.LinkAction"] [@@bs.val]
  end

  module PkgReq :
  sig
    type t
    val chain : t
    val chainOpt : t
    val entryTypes : t
  end = struct
    type t = int
    external chain : t = "Chain" [@@bs.module "HC.PkgReq"] [@@bs.val]
    external chainOpt : t = "ChainOpt" [@@bs.module "HC.PkgReq"] [@@bs.val]
    external entryTypes : t = "ChainOpt" [@@bs.module "HC.PkgReq"] [@@bs.val]
  end

  module ChainOpt :
  sig
    type t
    val chain : t
    val chainOpt : t
    val entryTypes : t
  end = struct
    type t = int
    external chain :
      t = "Chain" [@@bs.module "HC.PkgReq.ChainOpt"] [@@bs.val]
    external chainOpt :
      t = "ChainOpt" [@@bs.module "HC.PkgReq.ChainOpt"] [@@bs.val]
    external entryTypes :
      t = "ChainOpt" [@@bs.module "HC.PkgReq.EntryTypes"] [@@bs.val]
  end


  module Bridge :
  sig
    type t
    val from : t
    val to_ : t
  end = struct
    type t = int
    external from : t = "From" [@@bs.module "HC.Bridge"] [@@bs.val]
    external to_ : t = "To" [@@bs.module "HC.Bridge"] [@@bs.val]
  end

  module SysEntryType :
  sig
    type t
    val dna : t
    val agent : t
    val key : t
    val headers : t
    val del : t
  end =
  struct
    type t = int
    external dna : t = "DNA" [@@bs.module "HC.SysEntryType"] [@@bs.val]
    external agent : t = "Agent" [@@bs.module "HC.SysEntryType"] [@@bs.val]
    external key : t = "Key" [@@bs.module "HC.SysEntryType"] [@@bs.val]
    external headers : t = "Headers" [@@bs.module "HC.SysEntryType"] [@@bs.val]
    external del : t = "Del" [@@bs.module "HC.SysEntryType"] [@@bs.val]
  end

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
    external hash : string =
      "Hash" [@@bs.module "App.DNA"] [@@bs.val]

    type hash = [`DNA] HashString.t
    let hash : hash = (HashString.create hash :> hash)
  end

  (** Holochain's Agent related constants *)
  module Agent = struct

    external hash : string =
      "Hash" [@@bs.module "App.Agent"] [@@bs.val]

    type hash = [`Agent] HashString.t
 
    (** Holds your peer's identity info on the DHT. This is the hash for the
     * second entry (identity info) on your chain. **)
    let hash : hash = (HashString.create hash :> hash)

    external topHash : string = "TopHash" [@@bs.module "App.Agent"] [@@bs.val]

    (** Holds the most recent agent indentity entry that has been committed to
     * the chain. To start with its value is equivalent to App.Agent.Hash after
     * a call to updateAgent it will have the value of the newly committed
     * agent entry. *)
    let topHash : [`Top] HashString.t = HashString.create topHash

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
    type hash = [`Key] HashString.t
    let hash : hash = (HashString.create hash :> hash)
 end

end


