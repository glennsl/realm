let id x =
  x

let always x =
  fun _ -> x

let rec never x =
  never x

let tap f =
  fun x -> f x; x

let (>>) = ()
let (<<) = ()
let (|>) = (|>)
let (<|) = (@@)