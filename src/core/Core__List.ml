
(* Create *)
let singleton = ()
let repeat = ()
let range = ()
let cons = ()

(* Transform *)
let map = List.map
let mapi = ()
let foldl = List.fold_left
let foldr = List.fold_right
let filter = List.filter
let filterMap = fun f l -> Belt.List.keepMap l f

(* Utilities *)
let length = List.length
let reverse = ()
let has = () (* contains, member *)
let all = List.for_all
let any = () (* some *)
let max = ()
let min = ()
let sum = ()
let product = ()

(* Combine *)
let append = ()
let concat = ()
let concatMap = ()
let intersperse = ()
let map2 = ()
let map3 = ()
let map4 = ()
let map5 = ()

(* Sort *)
let sort = ()
let sortBy = ()
let sortWith = ()

(* Deconstruct *)
let isEmpty = ()
let head = ()
let tail = ()
let take = ()
let drop = ()
let partition = ()
let unzip = ()

let (++) = (@)