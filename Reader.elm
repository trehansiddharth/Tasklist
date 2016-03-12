module Reader where

type alias Reader r a = r -> a

lift : a -> Reader r a
lift = always

with : (r -> s) -> Reader s a -> Reader r a
with = (>>)

apply : Reader r (a -> b) -> Reader r a -> Reader r b
apply f x = \r -> f r (x r)

apply2 : Reader r (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
apply2 f x y = apply (apply f x) y

apply3 : Reader r (a -> b -> c -> d) -> Reader r a -> Reader r b -> Reader r c -> Reader r d
apply3 f x y z = apply (apply (apply f x) y) z

apply4 : Reader r (a -> b -> c -> d -> e) -> Reader r a -> Reader r b -> Reader r c -> Reader r d -> Reader r e
apply4 f w x y z = apply (apply (apply (apply f w) x) y) z

map : (a -> b) -> Reader r a -> Reader r b
map = (<<) apply lift

map2 : (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
map2 = (<<) apply2 lift

map3 : (a -> b -> c -> d) -> Reader r a -> Reader r b -> Reader r c -> Reader r d
map3 = (<<) apply3 lift

map4 : (a -> b -> c -> d -> e) -> Reader r a -> Reader r b -> Reader r c -> Reader r d -> Reader r e
map4 = (<<) apply4 lift

(<$>) : (a -> b) -> Reader r a -> Reader r b
(<$>) = map

(<*>) : Reader r (a -> b) -> Reader r a -> Reader r b
(<*>) = apply
