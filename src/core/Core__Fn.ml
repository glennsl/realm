let id x =
  x

let always x =
  fun _ -> x

let rec never x =
  never x

let tap f =
  fun x -> f x; x

let (>>) f g x = g (f x)
let (<<) f g x = f (g x)
let (|>) = (|>)
let (<|) = (@@)