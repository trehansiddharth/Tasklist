module Layout where
import Graphics.Element as G
import Signal as S
import Color as C
import Window as W
import Reactive as R
import Reader as D

type alias Dimensions = (Int, Int)
type alias Config m = { dimensions : Dimensions, model : m }
type alias LayoutOnDimensions = D.Reader Dimensions G.Element
type alias Layout m = R.Reactive (D.Reader (Config m) G.Element)

type alias Alignment = LayoutOnDimensions -> LayoutOnDimensions -> LayoutOnDimensions

left : Int -> Alignment
left width layout1 layout2 = \(x, y) -> G.beside (layout1 (width, y)) (layout2 (x - width, y))

right : Int -> Alignment
right width layout1 layout2 = \(x, y) -> (flip G.beside) (layout1 (width, y)) (layout2 (x - width, y))

above : Int -> Alignment
above height layout1 layout2 = \(x, y) -> G.above (layout1 (x, height)) (layout2 (x, y - height))

below : Int -> Alignment
below height layout1 layout2 = \(x, y) -> G.below (layout1 (x, height)) (layout2 (x, y - height))

custom : (LayoutOnDimensions -> LayoutOnDimensions -> LayoutOnDimensions) -> Alignment
custom = identity

map : R.Reactive (G.Element -> G.Element) -> Layout m -> Layout m
map = R.apply << (R.map (<<))

placeholder : anything -> Layout m
placeholder thing = inset (R.static 1) (R.static 1) <| map (R.static <| G.color C.green) <| embed (R.static G.middle) <| R.static <| always <| G.show thing

contain : R.Reactive Int -> R.Reactive Int -> R.Reactive G.Position -> Layout m -> Layout m
contain = R.map4 (\ex ey position g -> \config -> case config.dimensions of
    (x, y) -> G.container x y position (g { dimensions = (ex, ey), model = config.model }))

embed : R.Reactive G.Position -> R.Reactive (D.Reader m G.Element) -> Layout m
embed = R.map2 (\position element -> \config -> case config.dimensions of
    (x, y) -> G.container x y position (element config.model))

sized : R.Reactive (D.Reader m G.Element) -> Layout m
sized = R.map (\element -> \config -> case config.dimensions of
    (x, y) -> G.size x y (element config.model))

inset : R.Reactive Int -> R.Reactive Int -> Layout m -> Layout m
inset = R.map3 (\dx dy g -> \config -> let
        adjustedConfig x y = { config | dimensions = (max 0 (x - 2 * dx), max 0 (y - 2 * dy)) }
    in case config.dimensions of
        (x, y) -> G.container x y G.middle (g <| adjustedConfig x y))

combine : R.Reactive Alignment -> R.Reactive (m -> (m1, m2)) -> Layout m1 -> Layout m2 -> Layout m
combine = R.map4 (\alignment split layout1 layout2 -> \config -> alignment
    (\(x, y) -> layout1 { dimensions = (x, y), model = (fst << split) config.model })
    (\(x, y) -> layout2 { dimensions = (x, y), model = (snd << split) config.model })
    config.dimensions)

get : R.Reactive Int -> R.Reactive Int -> R.Reactive m -> Layout m -> R.Reactive G.Element
get = R.map4 (\x y m g -> g { dimensions = (x, y), model = m } )

fillscreen : R.Reactive m -> Layout m -> R.Reactive G.Element
fillscreen model = get (R.dynamic W.width) (R.dynamic W.height) model
