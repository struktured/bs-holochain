type type_ = string (* TODO make enum *)
  (* object or string *)

type properties =
  {description:string;
   type_:type_
  }

type t =
  {
    title:string;
    type_:type_;
    properties : properties
  }
