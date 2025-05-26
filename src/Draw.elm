module Draw exposing
    ( Drawing
    , fill
    , flatten
    , over
    , rect
    , render
    , rotate
    , scale
    , shift
    )

import Element as E exposing (Color, Element)
import Html exposing (Html)
import Linear exposing (Transform, Vector)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Util


type Drawing msg
    = Drawing (Transform -> Color -> Svg msg)


render : Float -> Float -> Float -> Float -> List (E.Attribute msg) -> Drawing msg -> Element msg
render x y w h attrs (Drawing drawing) =
    let
        t =
            Linear.shift -x -y
    in
    [ drawing t (E.rgb 0 0 0) ]
        |> Svg.svg [ SvgA.width (String.fromFloat w), SvgA.height (String.fromFloat h) ]
        |> E.html


empty : Drawing msg
empty =
    Drawing
        (\_ _ ->
            --Svg.circle [ SvgA.cx "0", SvgA.cy "0", SvgA.fill "red", SvgA.r "10" ] []
            Svg.circle [] []
        )



-- Primatives


line : Float -> Drawing msg
line thickness =
    Debug.todo "line"


circ : Float -> Float -> Float -> Drawing msg
circ x y r =
    Debug.todo "circ"


rect : Float -> Float -> Float -> Float -> Drawing msg
rect x y w h =
    Drawing <|
        \transform color ->
            let
                tl =
                    Linear.transform transform { x = x, y = y }

                tr =
                    Linear.transform transform { x = x + w, y = y }

                bl =
                    Linear.transform transform { x = x, y = y + h }

                br =
                    Linear.transform transform { x = x + w, y = y + h }

                toStr v =
                    String.fromFloat v.x ++ "," ++ String.fromFloat v.y
            in
            Svg.polygon
                [ SvgA.points <| String.join " " <| List.map toStr [ tl, tr, br, bl ]
                , SvgA.fill (Util.toHexString color)
                , SvgA.strokeWidth "2"
                , SvgA.stroke "#000000"
                ]
                []



-- Combinations


over : Drawing msg -> Drawing msg -> Drawing msg
over (Drawing d1) (Drawing d2) =
    Drawing <|
        \t c ->
            Svg.g []
                [ d1 t c
                , d2 t c
                ]


flatten : List (Drawing msg) -> Drawing msg
flatten =
    List.foldl over empty



-- Translations


shift : Float -> Float -> Drawing msg -> Drawing msg
shift x y (Drawing drawing) =
    Drawing <|
        \transform ->
            drawing <| Linear.compose (Linear.shift x y) transform


rotate : Float -> Drawing msg -> Drawing msg
rotate amount (Drawing drawing) =
    Drawing <|
        \transform ->
            drawing <| Linear.compose (Linear.rotation -amount) transform


scale : Float -> Float -> Drawing msg -> Drawing msg
scale x y (Drawing drawing) =
    Drawing <|
        \transform ->
            drawing <| Linear.compose (Linear.scale x y) transform



-- Styles


fill : Color -> Drawing msg -> Drawing msg
fill color (Drawing drawing) =
    Drawing <| \t _ -> drawing t color
