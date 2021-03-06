module Reactive where
import Signal as S

type Reactive a = Static a | Dynamic (Signal a)

static : a -> Reactive a
static = Static

dynamic : Signal a -> Reactive a
dynamic = Dynamic

getStatic : Reactive a -> a
getStatic x = case x of
    Static x' -> x'
    Dynamic x' -> Debug.crash "You ran getStatic on a dynamic reactive value, not good"

get : Reactive a -> Signal a
get x = case x of
    Static x' -> S.constant x'
    Dynamic x' -> x'

lift : a -> Reactive a
lift = static

apply : Reactive (a -> b) -> Reactive a -> Reactive b
apply rf rx = case rf of
    Static f -> case rx of
        Static x -> Static <| f x
        Dynamic x -> Dynamic <| S.map (\x' -> f x') x
    Dynamic f -> case rx of
        Static x -> Dynamic <| S.map (\f' -> f' x) f
        Dynamic x -> Dynamic <| S.map2 (\f' x' -> f' x') f x

apply2 : Reactive (a -> b -> c) -> Reactive a -> Reactive b -> Reactive c
apply2 f x y = apply (apply f x) y

apply3 : Reactive (a -> b -> c -> d) -> Reactive a -> Reactive b -> Reactive c -> Reactive d
apply3 f x y z = apply (apply (apply f x) y) z

apply4 : Reactive (a -> b -> c -> d -> e) -> Reactive a -> Reactive b -> Reactive c -> Reactive d -> Reactive e
apply4 f w x y z = apply (apply (apply (apply f w) x) y) z

map : (a -> b) -> Reactive a -> Reactive b
map = (<<) apply lift

map2 : (a -> b -> c) -> Reactive a -> Reactive b -> Reactive c
map2 = (<<) apply2 lift

map3 : (a -> b -> c -> d) -> Reactive a -> Reactive b -> Reactive c -> Reactive d
map3 = (<<) apply3 lift

map4 : (a -> b -> c -> d -> e) -> Reactive a -> Reactive b -> Reactive c -> Reactive d -> Reactive e
map4 = (<<) apply4 lift

(<$>) : (a -> b) -> Reactive a -> Reactive b
(<$>) = map

(<*>) : Reactive (a -> b) -> Reactive a -> Reactive b
(<*>) = apply
