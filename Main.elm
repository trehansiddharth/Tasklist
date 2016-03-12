module Main where
import Layout as L
import Graphics.Element as G
import Color as C
import Signal as S
import Reactive as R

main = R.get <| L.fillscreen <| L.combine (R.static <| L.left 40) (L.placeholder "he") (L.placeholder "she")
