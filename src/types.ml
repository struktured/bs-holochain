type hash_string = string

type link = {
  base : hash_string [@bs.as "Base"];
  link : hash_string [@bs.as "Link"];
  tag : string [@bs.as "Tag"];
} [@@bs.deriving abstract]
