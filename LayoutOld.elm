module LayoutOld (map,
    mapS,
    placeholder,
    embed,
    embedS,
    above,
    below,
    left,
    right,
    sized,
    inset,
    outline,
    withDimensions,
    withDimensionsS,
    fillscreen) where
import Graphics.Element as G
import Signal as S
import Color as C
import Window as W

type alias Config = { dimensions : (Int, Int) }
type Layout = Static (Config -> G.Element) | Dynamic (Signal Config -> Signal G.Element)

adjust : (Config -> Config) -> (G.Element -> G.Element) -> Layout -> Layout
adjust fc fe layout = case layout of
    (Static g) -> Static (fe << g << fc)
    (Dynamic g) -> Dynamic (S.map fe << g << S.map fc)

adjustS : (S.Signal Config -> S.Signal Config) -> (S.Signal G.Element -> S.Signal G.Element) -> Layout -> Layout
adjustS fc fe layout = case layout of
    (Static g) -> Dynamic (fe << S.map g << fc)
    (Dynamic g) -> Dynamic (fe << g << fc)

map : (G.Element -> G.Element) -> Layout -> Layout
map = adjust identity

mapS : (S.Signal G.Element -> S.Signal G.Element) -> Layout -> Layout
mapS = adjustS identity

placeholder : anything -> Layout
placeholder thing = inset 1 1 <| map (G.color C.green) <| contain G.middle <| G.show thing

{--contain : Int -> Int -> G.Position -> Layout -> Layout
contain width height position layout = case layout of
    Static g -> Static <| \config -> case config.dimensions of
        (x, y) -> G.container x y position <| staticWithDimensions x y layout
    Dynamic g -> Dynamic <| \configSignal -> () configSignal (Signal.map (\config -> config.dimensions))--}

embed : G.Position -> G.Element -> Layout
embed position element = Static (\config -> case config.dimensions of
    (x, y) -> G.container x y position element)

embedS : Signal G.Position -> Signal G.Element -> Layout
embedS positionSignal elementSignal = Dynamic (\configSignal -> S.map3 (\config position element -> case config.dimensions of
    (x, y) -> G.container x y position element) configSignal positionSignal elementSignal)

sized : G.Element -> Layout
sized element = Static <| \config -> case config.dimensions of
    (x, y) -> G.size x y element

sizedS : Signal G.Element -> Layout
sizedS signalElement = Dynamic <| \signalConfig -> Signal.map2 (\element config -> case config.dimensions of
    (x, y) -> G.size x y element) signalElement signalConfig

inset : Int -> Int -> Layout -> Layout
inset dx dy layout = let
    makeConfig config = case config.dimensions of
        (x, y) -> { dimensions = (max 0 (x - 2 * dx), max 0 (y - 2 * dy))}
    makeContainer config element = case config.dimensions of
        (x, y) -> G.container x y G.middle element
    in case layout of
        Static g -> Static (\config -> makeContainer config <| g <| makeConfig <| config)
        Dynamic g -> Dynamic (\configSignal -> S.map2 makeContainer configSignal <| g <| S.map makeConfig <| configSignal)

outline : Int -> C.Color -> Layout -> Layout
outline i c = map (G.color c) << inset i i

above : Int -> Layout -> Layout -> Layout
above height layout1 layout2 = case (layout1, layout2) of
    (Static g, Static h) -> Static (\config -> case config.dimensions of
        (x, y) -> G.above (g { dimensions = (x, height) }) (h { dimensions = (x, y - height) }))
    (Static g, Dynamic h) -> Dynamic (\configSignal -> S.map2 G.above (S.map (\config -> g <| case config.dimensions of
        (x, y) -> { dimensions = (x, height) }) configSignal) (h <| S.map (\config -> case config.dimensions of
        (x, y) -> { dimensions = (x, y - height) }) configSignal))
    (Dynamic g, Static h) -> Dynamic (\configSignal -> S.map2 G.above (g <| S.map (\config -> case config.dimensions of
        (x, y) -> {dimensions = (x, height)}) configSignal) (S.map (\config -> h <| case config.dimensions of
            (x, y) -> { dimensions = (x, y - height)}) configSignal))
    (Dynamic g, Dynamic h) -> Dynamic (\configSignal -> S.map2 G.above (g <| S.map (\config -> case config.dimensions of
        (x, y) -> {dimensions = (x, height)}) configSignal) (h <| S.map (\config -> case config.dimensions of
        (x, y) -> { dimensions = (x, y - height) }) configSignal))

below : Int -> Layout -> Layout -> Layout
below height layout1 layout2 = case (layout1, layout2) of
    (Static g, Static h) -> Static (\config -> case config.dimensions of
        (x, y) -> G.below (g { dimensions = (x, height) }) (h { dimensions = (x, y - height) }))
    (Static g, Dynamic h) -> Dynamic (\configSignal -> S.map2 G.below (S.map (\config -> g <| case config.dimensions of
        (x, y) -> { dimensions = (x, height) }) configSignal) (h <| S.map (\config -> case config.dimensions of
        (x, y) -> { dimensions = (x, y - height) }) configSignal))
    (Dynamic g, Static h) -> Dynamic (\configSignal -> S.map2 G.below (g <| S.map (\config -> case config.dimensions of
        (x, y) -> {dimensions = (x, height)}) configSignal) (S.map (\config -> h <| case config.dimensions of
            (x, y) -> { dimensions = (x, y - height)}) configSignal))
    (Dynamic g, Dynamic h) -> Dynamic (\configSignal -> S.map2 G.below (g <| S.map (\config -> case config.dimensions of
        (x, y) -> {dimensions = (x, height)}) configSignal) (h <| S.map (\config -> case config.dimensions of
        (x, y) -> { dimensions = (x, y - height) }) configSignal))

left : Int -> Layout -> Layout -> Layout
left width layout1 layout2 = case (layout1, layout2) of
    (Static g, Static h) -> Static (\config -> case config.dimensions of
        (x, y) -> G.beside (g { dimensions = (width, y) }) (h { dimensions = (x - width, y) }))
    (Static g, Dynamic h) -> Dynamic (\configSignal -> S.map2 G.beside (S.map (\config -> g <| case config.dimensions of
        (x, y) -> { dimensions = (width, y) }) configSignal) (h <| S.map (\config -> case config.dimensions of
        (x, y) -> { dimensions = (x - width, y) }) configSignal))
    (Dynamic g, Static h) -> Dynamic (\configSignal -> S.map2 G.beside (g <| S.map (\config -> case config.dimensions of
        (x, y) -> {dimensions = (width, y)}) configSignal) (S.map (\config -> h <| case config.dimensions of
            (x, y) -> { dimensions = (x - width, y)}) configSignal))
    (Dynamic g, Dynamic h) -> Dynamic (\configSignal -> S.map2 G.beside (g <| S.map (\config -> case config.dimensions of
        (x, y) -> {dimensions = (width, y)}) configSignal) (h <| S.map (\config -> case config.dimensions of
        (x, y) -> { dimensions = (x - width, y) }) configSignal))

right : Int -> Layout -> Layout -> Layout
right width layout1 layout2 = case (layout1, layout2) of
    (Static g, Static h) -> Static (\config -> case config.dimensions of
        (x, y) -> flip G.beside (g { dimensions = (width, y) }) (h { dimensions = (x - width, y) }))
    (Static g, Dynamic h) -> Dynamic (\configSignal -> S.map2 (flip G.beside) (S.map (\config -> g <| case config.dimensions of
        (x, y) -> { dimensions = (width, y) }) configSignal) (h <| S.map (\config -> case config.dimensions of
        (x, y) -> { dimensions = (x - width, y) }) configSignal))
    (Dynamic g, Static h) -> Dynamic (\configSignal -> S.map2 (flip G.beside) (g <| S.map (\config -> case config.dimensions of
        (x, y) -> {dimensions = (width, y)}) configSignal) (S.map (\config -> h <| case config.dimensions of
            (x, y) -> { dimensions = (x - width, y)}) configSignal))
    (Dynamic g, Dynamic h) -> Dynamic (\configSignal -> S.map2 (flip G.beside) (g <| S.map (\config -> case config.dimensions of
        (x, y) -> {dimensions = (width, y)}) configSignal) (h <| S.map (\config -> case config.dimensions of
        (x, y) -> { dimensions = (x - width, y) }) configSignal))

staticWithDimensions : Int -> Int -> Layout -> G.Element
staticWithDimensions width height layout = case layout of
    Static g -> g { dimensions = (width, height) }
    Dynamic g -> _

withDimensions : Int -> Int -> Layout -> Signal G.Element
withDimensions width height layout = case layout of
    Static g -> S.constant <| g { dimensions = (width, height) }
    Dynamic g -> g <| S.constant { dimensions = (width, height) }

withDimensionsS : S.Signal Int -> S.Signal Int -> Layout -> Signal G.Element
withDimensionsS widthSignal heightSignal layout = case layout of
    Static g -> S.map g <| Signal.map2 (\x y -> { dimensions = (x, y)}) widthSignal heightSignal
    Dynamic g -> g <| Signal.map2 (\x y -> { dimensions = (x, y)}) widthSignal heightSignal

fillscreen : Layout -> Signal G.Element
fillscreen = withDimensionsS W.width W.height
