module M =
  Map.Make
    (struct
      type t = string
      let compare = compare
    end)
include M
