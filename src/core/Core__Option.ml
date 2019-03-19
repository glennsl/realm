
(* Common Helpers *)
let withDefault default =
  function
  | Some value  -> value
  | None        -> default

let map f =
  function
  | Some value  -> Some (f value)
  | None        -> None

let map2 = ()
let map3 = ()
let map4 = ()
let map5 = ()

(* Chaining Options *)
let andThen f =
  function
  | Some value  -> f value
  | None        -> None
