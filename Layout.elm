module Layout where
import Graphics.Element as G
import Signal as S
import Color as C
import Window as W
import Reactive as R
import Reader as D

type alias Config = { dimensions : (Int, Int) }
type alias SimpleLayout = D.Reader Config G.Element
type alias Layout = R.Reactive SimpleLayout

type Alignment = Above Int | Below Int | Left Int | Right Int | Custom (SimpleLayout -> SimpleLayout -> SimpleLayout)

above : Int -> Alignment
above = Above

below : Int -> Alignment
below = Below

left : Int -> Alignment
left = Left

right : Int -> Alignment
right = Right

custom : (SimpleLayout -> SimpleLayout -> SimpleLayout) -> Alignment
custom = Custom

map : R.Reactive (G.Element -> G.Element) -> Layout -> Layout
map = R.apply << (R.map (<<))

placeholder : anything -> Layout
placeholder thing = inset (R.static 1) (R.static 1) <| map (R.static <| G.color C.green) <| embed (R.static G.middle) <| R.static <| G.show thing

contain : R.Reactive Int -> R.Reactive Int -> R.Reactive G.Position -> Layout -> Layout
contain rx ry rposition layout = embed rposition (withDimensions rx ry layout)

embed : R.Reactive G.Position -> R.Reactive G.Element -> Layout
embed = R.map2 (\position element -> \config -> case config.dimensions of
    (x, y) -> G.container x y position element)

sized : R.Reactive G.Element -> Layout
sized = R.map (\element -> \config -> case config.dimensions of
    (x, y) -> G.size x y element)

inset : R.Reactive Int -> R.Reactive Int -> Layout -> Layout
inset = R.map3 (\dx dy g -> \config -> let
        adjustedConfig x y = { config | dimensions = (max 0 (x - 2 * dx), max 0 (y - 2 * dy)) }
    in case config.dimensions of
        (x, y) -> G.container x y G.middle (g <| adjustedConfig x y))

combine : R.Reactive Alignment -> Layout -> Layout -> Layout
combine = R.map3 (\alignment layout1 layout2 -> \config -> let
        makeConfig (x', y') = { config | dimensions = (x', y') }
    in case config.dimensions of
        (x, y) -> case alignment of
            Left width -> G.beside (layout1 <| makeConfig (width, y)) (layout2 <| makeConfig (x - width, y))
            Right width -> (flip G.beside) (layout1 <| makeConfig (width, y)) (layout2 <| makeConfig (x - width, y))
            Above height -> G.above (layout1 <| makeConfig (x, height)) (layout2 <| makeConfig (x, y - height))
            Below height -> G.below (layout1 <| makeConfig (x, height)) (layout2 <| makeConfig (x, y - height))
            Custom alignmentFunction -> alignmentFunction layout1 layout2 config)

withDimensions : R.Reactive Int -> R.Reactive Int -> Layout -> R.Reactive G.Element
withDimensions = R.map3 (\x y g -> g { dimensions = (x, y) } )

fillscreen : Layout -> R.Reactive G.Element
fillscreen = withDimensions (R.dynamic W.width) (R.dynamic W.height)
