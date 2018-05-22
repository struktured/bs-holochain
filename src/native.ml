type hash_string = string

(** Returns an application property, which are defined by the app developer. It returns externalues from the DNA file that you set as properties of your application (e.g. Name, Language, Description, Author, etc.). *)
external property : string -> string (*or_error *) = "property" [@@bs.val]

(** Use e this function to make a hash of the given entry data. This is the same hash externalue that would be returned if entryData were passed to commit and by which an entry of this type would be retrievable from the DHT using get. The type of the entryData parameter depends on the entry format of entry. If it's a string entry format then the type must be string. If it's a JSON entry format, then it can be any type, and the externalue will get appropriately converted to JSON. If it is a links format entry, then the type must by a JSON object. *)
external make_hash :
  entry_type:string -> 'entry_data ->
  hash_string (*or_error *) = "makeHash" [@@bs.val]

(** Sends output to the debugging log. The type of externalue is arbitrary and will get converted to a string according toteh language conversion limitations. *)
external debug : 'a -> unit = "debug" [@@bs.val]

(** Use the agent's private key to sign some contents *)
external sign : string -> string = "sign" (* or_error *) [@@bs.val]

(** Uses the signature, data and signatory's public key to verify the sign in contents of data. Result represents whether its a match or not. pubKeyshould be a public key. *)
external verify_signature :
  signature:string ->
  data:string ->
  pub_key:string ->
  bool (* or_error *) =
  "verifySignature"


(** Attempts to commit an entry to your local source chain. It will cause callback to your validateCommitfunction. Returns either an error or the hash of the committed entry upon success. The type of the entryData parameter depends on the entry format of entry. If it's a string entry format then the type must be string. If it's a JSON entry format, then it can by any type, and the value will get appropriately converted to JSON. If it is a links format entry, then the type must by a JSON object.

A linksentry object looks like this

{ Links: [ { Base: "2bDja...", Link: "Fb4aXa...", Tag: "links to" } ] }
Base and Linkmust both be type hash. Tagcan be any string, describing the relationship between Base and Link. Tagwill later be used in getLinks. It may optionally contain a 4th property LinkActionwhich should be set to HC.LinkAction.Delin order to mark the link as deleted. See the examples below.
*)
external commit :
  entry_type:string ->
  'any_type ->
  hash_string (*or_error*) =
  "commit"

(** Calls an exposed function from another zome. [arguments] is a string or an object depending on the [CallingType] that was specified in the function's definition in the DNA. Returns the externalue that's returned by the given function *)
external call : zome_name:string -> function_name:string -> 'obj Js.t ->
  'value (*or_error *)= "call" [@@bs.val]

(** Calls a bridged function from another app. [app_dna_hash] is the application being called. Note that the application must have explicitly been bridged. In development use hcdev's -bridgeSpecs and a bridge_specs.json file to setup bridging. Just like in send , the arguments parameter is a string or an object/hash depending on the CallingType that was specified in the function's definition. Returns the externalue that's returned by the given function on the other side of the bridge.
*)
external bridge :
  app_dna_hash:hash_string ->
  zome_name:string ->
  function_name:string ->
  'obj Js.t ->
  'any_type = "bridge" [@@bs.val]

(** The type of a bridge. If side is [`From] then [toApp] is non empty. If side
    is [`To], [token] is non empty. *)
class type bridge =
  object
    method toApp : hash_string [@@bs.set]
    method side:[`From | `To] [@@bs.set]
    method token:string [@@bs.set]
  end [@@bs]

(**This function allows your app to examine which bridges have been put in place. *)
external get_bridges :
  unit -> bridge array = "getBridges" [@@bs.val]
