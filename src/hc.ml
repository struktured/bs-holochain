(** Holochain constants *)
type hash_string = string
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





