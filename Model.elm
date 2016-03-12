module Model where

none : ()
none = ()

splitNone : () -> ((), ())
splitNone = always ((), ())

splitLike : (m -> m1, m -> m2) -> m -> (m1, m2)
splitLike (f, g) = \m -> (f m, g m)
