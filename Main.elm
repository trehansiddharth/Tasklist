module Main where
import Layout as L
import Graphics.Element as G
import Color as C
import Signal as S
import Reactive as R
import Model as M

main = R.get <| L.fillscreen (R.static M.none) layout

layout = L.combine (R.static <| L.left 40) (R.static M.splitNone) he she

he = L.placeholder "he"

she = L.placeholder "she"
