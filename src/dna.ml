type properties =
  {
    description: string;
    language: string (* TODO make enum *)
  }

type dht_config = {hashType:string (* TODO make enum *) }

type zome_function = {name:string;callingTyping:string} (* TODO make enum *)

type zome_entry =
  {
    name: string;
    dataFormat: string;
    sharing: string
  }

type zome =
  {
    name: string;
    description: string;
    codeFile: string;
    ribosomeType: string (* TODO make enum *);
    entries: zome_entry list;
    functions : zome_function list
  }

type t =
  {
    version: int;
    uuid: string;
    name: string;
    propertiesSchemaFile: string;
    requiresVersion: int;
    dHTConfig: dht_config;
    zomes: zome list;
  }
