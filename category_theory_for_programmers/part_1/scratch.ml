let id x = x

let compose f g x = g (f x)

let memoize f =
  let store = Hashtbl.create 100 in
  fun x ->
    match Hashtbl.find_opt store x with
    | Some(y) -> 
        print_string "Found it!";
        y
    | None -> 
        let y = f x in
        Hashtbl.add store x y;
        y

(* Writing out all the bool functions, match check is not happy *) 
let true_to_false true = false
let true_to_true true = true
let false_to_true false = true
let false_to_false false = false

module type Monoid = sig
  type a

  val mempty : a
  val mappend : a -> a -> a
end

module StringMonoid : Monoid with type a = string = struct
  type a = string

  let mempty = ""
  let mappend = (^)
end

module IntMonoid : Monoid with type a = int = struct
  type a = int

  let mempty = 0
  let mappend = (+)
end

type 'a writer = 'a * string
let ( >=> ) f g a = 
  let (b, s1) = f a in
  let (c, s2) = g b in
  (c, s1 ^ s2)

let fst (a, _) = a
let snd (_, b) = b
  
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

module IntProduct = struct
  let i x = x
  let j x = if x then 0 else 1

  let m = function
    | Left(x) -> i x
    | Right(x) -> j x
end

module Maybe = struct
  type 'a t = 'a option

  let fmap f = function
    | None -> None
    | Some(x) -> Some(f x)
end

module Reader = struct
  type ('r, 'a) t = 'r -> 'a

  let fmap f g = compose f g
end

module Either = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b

  let bimap f g = function
    | Left(x) -> Left(f x)
    | Right(y) -> Right(g y)
end

module Pair = struct
  type ('a, 'b) t = 'a * 'b

  let bimap f g (a, b) = (f a, g b)
  let first f = bimap f id
  let second = bimap id
end
