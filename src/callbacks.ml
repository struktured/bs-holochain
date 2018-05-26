open Types
module type REQUIRED = sig

    (** Each zome must include this function, which is called during system genesis. It executes just after the initial genesis entries are committed to your chain (1st - DNA entry, 2nd Identity entry). It enables you specify any additional operations you want performed when a node joins your holochain, for example, you may want to add some user/address/key information to the DHT to announce the presence of this new node. This function must return true if it is to succeed, and the application to start successfully. *)
    val genesis : unit -> bool


    (** This function gets called when an entry is about to be committed to a source chain. Use this function to describe the agreements about data as it should be added to shared Holochain. This function gets called for all entry types. For more background, read the Validation Functions section. *)
    val validateCommit : entry_type:'obj Js.t -> entry:'any_type Js.t ->
      package:'package_obj Js.t -> sources:string array -> bool

    (** This function gets called when an entry is about to be committed to the DHT on any node. It is very likely that this validation routine should check the same data integrity as validateCommit, but, as it happens during a different part of the data life-cycle, it may require additional validation steps. This function will only get called on entry types with "public" sharing, as they are the only types that get put to the DHT by the system. For more background, read the Validation Functions section. *)
    val validatePut :
      entry_type:string ->
      entry:'any_type Js.t ->
      header:'header_obj Js.t ->
      package:'package_obj Js.t ->
      sources:string array -> bool

    (** This function gets called as a consequence of a mod command being issued.
        replaces is the hash of the entry being replaced. For more background,
        read the Validation Functions section.
    *)
    val validateMod :
      entry_type:string (* enum? *) ->
      entry:'any_type Js.t ->
      header:'header_obj Js.t ->
      replaces:hash_string ->
      package:'package_obj Js.t ->
      sources:string array -> bool

    (** This function gets called as a consequence of a del command being issued. For more background, read the Validation Functions section. *)
    val validateDel :
      entry_type:string (* enum? *) ->
      hash:hash_string ->
      package:'package_obj Js.t ->
      sources:string array -> bool



    (** This function gets called when ever links are being written to the DHT. Links are added for every linking element in the special "links" entry type. Note that this is a DHT level validation routine, in that it gets called when the Link message is received by a DHT node, not when the linking entry is committed. The regular validateCommit routine gets called as usual when that linking entry is committed to the source chain. For more background, read the Validation Functions section. *)
    val validateLink:
      entry_type:string (* enum? *) ->
      hash:hash_string ->
      links:'link_object Js.t array ->
      package:'package_obj Js.t ->
      sources:string array -> bool

    (** This function should simply return nil if the data required by its corresponding validation function is just the minimum default of the Entry and Header of the action. Otherwise this function must return a "Package Request" object, which specifies what data to be sent to the validating node. For more background, read the Validation Packaging section.

        Note that a commit action will trigger a call to validatePutPkg locally when committing happens as validateCommit must have the same data available to it as does validatePut. *)
    val validatePutPkg : entry_type:string -> 'pkg_req_obj Js.t (* or nil *)

    (** This function should simply return nil if the data required by its corresponding validation function is just the minimum default of the Entry and Header of the action. Otherwise this function must return a "Package Request" object, which specifies what data to be sent to the validating node. For more background, read the Validation Packaging section. *)
    val validateModPkg :
      entry_type:string -> 'pkg_req_obj Js.t (* or nil *)

    (** This function should simply return nil if the data required by its corresponding validation function is just the minimum default of the Entry and Header of the action. Otherwise this function must return a "Package Request" object, which specifies what data to be sent to the validating node. For more background, read the Validation Packaging section. *)
    val validateDelPkg:
      entry_type:string -> 'pkg_req_obj Js.t (* or nil *)

    (** This function should simply return nil if the data required by its corresponding validation function is just the minimum default of the Entry and Header of the action. Otherwise this function must return a "Package Request" object, which specifies what data to be sent to the validating node. For more background, read the Validation Packaging section. *)
    val validateLinkPkg:
      entry_type:string -> 'pkg_req_obj Js.t (* or nil *)

  end


  (** Optional callbacks *)
  module type OPTIONAL = sig
    (** All zomes which expose functions for bridging from other applications MUST also define a bridgeGenesis function (i.e. the "Bridge-To" side). Zomes which want to call functions in other applications MAY define a bridgeGenesis function and declare that they do so by setting the Zome.BridgeTo value in their DNA. *)
    val bridgeGenesis : side:([`From|`To] [@bs.int]) -> dna:hash_string ->
      app_data:string -> bool

    (** This function gets called by the system when a message is received by a node. The return value of the function will be sent back to the sender and will be the result of the send function that sent the message. The value you return from this function will be sent back to the node that sent you the message. *)
    val receive : hash_string -> message:'obj Js.t -> options:'obj Js.t ->
      'any_type Js.t
  end
