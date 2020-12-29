[<AutoOpen>]
module Core.Utils

let (^) f x = f x

let inline fst (a, _) = a
let inline snd (_, b) = b