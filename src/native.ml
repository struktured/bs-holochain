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


(** Attempts to commit an entry to your local source chain. It will cause callbac to your validaneCommitfunction. Returns either an error or the hash of the committed entry upon success. The type of the entryData parameter depends on the entry format of entry. If it's a string entry format then the type must be string. If it's a JSON entry format, then it can by any type, and the value will get appropriately converted to JSON. If it is a links format entry, then the type must by a JSON object.

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

(**
 *This function retrieves an entry from the local chain or the DHT. If options.StatusMask is present, it determines which entries to return, depending on their status. If options.GetMask is present, this option allows you to specify what information about the entry you want. For more on that, see Entry Objects and Masks.

If options.Local is set to true, it indicates that the get refers to the local chain only. This allows you to retrieve specific entries from your chain, which includes private entries that aren't published to the DHT.

If options.Bundle is set to true, it indicates that the get refers to the currently started bundle only. If no bundle has been started, returns an error.

If only StatusMask value specified or only Entry is specified the return value will be the actual entry value. Otherwise the return value will be an object with properties of the same name as the mask name.

hash: hash-string
options: object (optional)
options.StatusMask: Status-int
options.GetMask: Mask-int
options.Local: boolean
options.Bundle: boolean
Returns: Entry-object OR HC.HashNotFound
*)
external get :
  hash_string -> options:'a Js.t -> 'entry Js.t =
  "get" [@@bs.val]


(**Retrieves a list of links tagged as tag on base from the DHT. If tag is an empty string it will return all the links on the baseand the list will also include the Tag property on entries. With options as {Load: false} (which is the default) returns a list of the form [{Hash:"QmY..."},..] With options as {Load: true} it will get the entry values of the links and return a list of the form [{Hash:"QmY...",EntryType:"<entry-type>",Entry:"<entry value here>",Source:"<source-hash>"},..]}. Use options.StatusMask to return only links with a certain status. Default is to return only Live links. You can use defined constants HC.Status.Live/Deleted/Rejected as the int value. *)

external get_links  :
  base:hash_string -> tag:string -> options:'a Js.t -> 'entry Js.t array =
  "getLinks"

(** Commits a DelEntry to the local chain with given delete message, and, if the entry type of entry is not private, moves the entry to the Deleted status on the DHT. *)
external remove : entry:'obj Js.t -> message:string -> hash_string =
  "remove" [@@bs.val]

(** Attempts to commit an entry to your local source chain that "replaces" a previous entry. If entryType is not private, update will movereplaces to a Modifiedstatus on the DHT. Additionally the modification action will be recorded in the entries' header in the local chain, which will be used by validation routes. **)
external update : entry_type:string -> entry_data:'string_or_obj Js.t ->
  'string_or_obj Js.t = "update"

external update_agent :
  options: 'obj Js.t -> hash_string (* or-error *) = "updateAgent" [@@bs.val]


