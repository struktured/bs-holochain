Holochain API bindings via bucklescript. This library allows you to write a
holochain application in pure Ocaml/Reason.


# Dependencies

This project requires npm and bucklescript 4.0.1. To produce a complex library
you also need to unify the json into one big zome file. Rollup and babel are typical
choices.

# Build
```
make
```

# Install as a library

In your dependent project run

`npm install <this-project-directory>`


# How to use


## Define your zomes as usual

Create a dna.json and schema files as you would for any standard holochain app.

## Implement your zomes

For each zome you define a module that instantiates a builder style functor to
accumulate all the entry definitions.

```OCaml
(** user zome example *)
open HC

(* Instantiate a new zome builder with name equal to "myzome" *)
module Builder = Zome.Builder(struct let name = "myzome" end)

(* Define a named data type "mydata" as an abstract json record.
   Your dna.json would require a zome entry defined as well with the same name
   and shape.
*)
module MyData = struct
  let name = "mydata"
  type t = {data:string} [@@bs.deriving abstract]
end

(* Add the MyData entry type to the zome with validation functions
   that simply accepts any input (eg. returns true or Null,
   as the callback would require).

   The return moduled is an [Entry.S] signature, giving the developer
   specialized commit, update, and get functions over for the MyData entry.
*)
module MyDataEntry = Builder.Entry0(MyData)(Validate.Accept_all(MyData))


(** Define input type to a zome function as a zome hash string of [MyData.t] *)
type args = MyData.t HashString.t

(* Define a public zome function (this was defined in dna.json as
   function named "dataExists")
*)
let dataExists args =
  match MyDataEntry.get args with None -> false | Some _ -> true

(* This finalizes the zome and includes the validation functions so they now serve
   as javascript call back functions.

   Since a zome requires a genesis function and possibly a receiver function, so 
   does the below functor. In this example builtin modules are used for both. The
   [Genesis.Success] module just returns the true and does no initialize. Similarly,
   the send and receive functions operate on the unit value.
*)
include Builder.Build(Genesis.Success)(SendReceive.Unit)
```

See [NewCraigsLists, ported to OCaml](https://github.com/struktured/NewCraigsList) for a more complex implementation.
