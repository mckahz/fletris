module Draw exposing
    ( Drawing
    , behind
    , clip
    , empty
    , fill
    , flatten
    , image
    , over
    , rect
    , render
    , rotate
    , scale
    , shift
    , stroke
    , strokeWidth
    )

import Element as E exposing (Color, Element)
import Html exposing (Html)
import Html.Attributes exposing (href)
import Linear exposing (Transform, Vector)
import Svg as S
import Svg.Attributes as A
import Util


type Object msg
    = Svg (S.Svg msg)
    | PixelArt
        { palette : List Color
        , width : Int
        , pixels : List Int
        }
    | DrawingObject (Drawing msg)


type alias Rect =
    { x : Float, y : Float, w : Float, h : Float }


rectCorners : Rect -> List Vector
rectCorners r =
    let
        tl =
            { x = r.x, y = r.y }

        tr =
            { x = r.x + r.w, y = r.y }

        bl =
            { x = r.x, y = r.y + r.h }

        br =
            { x = r.x + r.w, y = r.y + r.h }
    in
    [ tl
    , bl
    , br
    , tr
    ]


pointsString : List Vector -> String
pointsString points =
    let
        toStr v =
            String.fromFloat v.x ++ "," ++ String.fromFloat v.y
    in
    points
        |> List.map toStr
        |> String.join " "


type alias Params =
    { transform : Transform
    , clip : Rect
    , fill : Color
    , stroke : Color
    , strokeWidth : Float
    }


type Drawing msg
    = Drawing (List (Params -> Object msg))



-- to avoid the seam between tiles, we scale up everything by a tiny amount.


epsilon : Float
epsilon =
    0.05


render : Float -> Float -> Float -> Float -> List (E.Attribute msg) -> Drawing msg -> Element msg
render x y w h attrs drawing =
    let
        invisible =
            E.rgba 0 0 0 0
    in
    S.svg [ A.width (String.fromFloat w), A.height (String.fromFloat h) ]
        [ drawing
            |> renderDrawing
                { transform =
                    Linear.shift -x -y
                , fill =
                    invisible
                , stroke =
                    invisible
                , strokeWidth = 0
                , clip =
                    let
                        frustomDiameter =
                            2000000
                    in
                    { x = -frustomDiameter / 2, y = -frustomDiameter / 2, w = frustomDiameter, h = frustomDiameter }
                }
        ]
        |> E.html
        |> E.el attrs


renderDrawing : Params -> Drawing msg -> S.Svg msg
renderDrawing params (Drawing drawing) =
    renderDrawingHelp 0 params (Drawing drawing)


renderDrawingHelp : Int -> Params -> Drawing msg -> S.Svg msg
renderDrawingHelp depth params (Drawing drawing) =
    drawing
        |> List.foldl
            (\lazyObject { d, objects } ->
                { d = d - 1
                , objects = renderObject d params (lazyObject params) :: objects
                }
            )
            { d = depth, objects = [] }
        |> .objects
        |> S.g
            []


renderObject : Int -> Params -> Object msg -> S.Svg msg
renderObject depth params object =
    case object of
        Svg svg ->
            svg

        DrawingObject drawing ->
            renderDrawingHelp depth params drawing

        PixelArt img ->
            Debug.todo "bmp"



-- Primatives


empty : Drawing msg
empty =
    Drawing []


image : String -> Drawing msg
image url =
    Drawing
        [ \params ->
            let
                origin =
                    Linear.transform params.transform { x = 0, y = 0 }

                unit =
                    Linear.getScale <|
                        Linear.compose
                            (let
                                s =
                                    1 / 16
                             in
                             Linear.scale s s
                            )
                            params.transform

                topLeft =
                    { x = origin.x - unit.x * (params.clip.x + params.clip.w / 2)
                    , y = origin.y - unit.y * (params.clip.y + params.clip.h / 2)
                    }
            in
            Svg <|
                S.image
                    [ A.xlinkHref url
                    , A.x <| String.fromFloat topLeft.x
                    , A.y <| String.fromFloat topLeft.y
                    , A.height <| String.fromFloat <| 16 * unit.y
                    , A.imageRendering "pixelated"
                    , let
                        format { x, y } =
                            String.fromFloat x ++ " " ++ String.fromFloat y
                      in
                      A.clipPath <|
                        (++) "polygon(" <|
                            String.join ","
                                (List.map (format << Linear.transform (Linear.scale unit.x unit.y))
                                    [ { x = params.clip.x, y = params.clip.y }
                                    , { x = params.clip.x + params.clip.w, y = params.clip.y }
                                    , { x = params.clip.x + params.clip.w, y = params.clip.y + params.clip.h }
                                    , { x = params.clip.x, y = params.clip.y + params.clip.h }
                                    ]
                                )
                                ++ ")"
                    ]
                    []
        ]


line : Float -> Drawing msg
line thickness =
    Debug.todo "line"


circ : Float -> Float -> Float -> Drawing msg
circ x y r =
    Debug.todo "circ"


rect : Rect -> Drawing msg
rect r =
    scale (1 + epsilon) (1 + epsilon) <|
        Drawing <|
            [ \params ->
                Svg <|
                    S.polygon
                        [ A.points <| pointsString <| List.map (Linear.transform params.transform) <| rectCorners r
                        , A.fill (Util.toHexString params.fill)
                        , A.strokeWidth <| String.fromFloat params.strokeWidth
                        , A.stroke (Util.toHexString params.stroke)
                        ]
                        []
            ]



-- Combinations


behind : Drawing msg -> Drawing msg -> Drawing msg
behind (Drawing d1) (Drawing d2) =
    Drawing <| List.append d1 d2


over : Drawing msg -> Drawing msg -> Drawing msg
over (Drawing d1) (Drawing d2) =
    Drawing <| List.append d2 d1


flatten : List (Drawing msg) -> Drawing msg
flatten =
    List.foldl over empty



-- Translations


shift : Float -> Float -> Drawing msg -> Drawing msg
shift x y (Drawing drawing) =
    Drawing <|
        List.map
            (\lazyObject params ->
                lazyObject { params | transform = Linear.compose (Linear.shift x y) params.transform }
            )
            drawing


rotate : Float -> Drawing msg -> Drawing msg
rotate amount (Drawing drawing) =
    Drawing <|
        List.map
            (\lazyObject params ->
                lazyObject { params | transform = Linear.compose (Linear.rotation -amount) params.transform }
            )
            drawing


scale : Float -> Float -> Drawing msg -> Drawing msg
scale x y (Drawing drawing) =
    Drawing <|
        List.map
            (\lazyObject params ->
                lazyObject { params | transform = Linear.compose (Linear.scale x y) params.transform }
            )
            drawing


clip : Rect -> Drawing msg -> Drawing msg
clip r (Drawing drawing) =
    Drawing <|
        List.map
            (\lazyObject params ->
                lazyObject { params | clip = r }
            )
            drawing



-- Styles


fill : Color -> Drawing msg -> Drawing msg
fill color (Drawing drawing) =
    Drawing <| List.map (\lazyObject params -> lazyObject { params | fill = color }) drawing


stroke : Color -> Drawing msg -> Drawing msg
stroke color (Drawing drawing) =
    Drawing <| List.map (\lazyObject params -> lazyObject { params | stroke = color }) drawing


strokeWidth : Float -> Drawing msg -> Drawing msg
strokeWidth width (Drawing drawing) =
    Drawing <| List.map (\lazyObject params -> lazyObject { params | strokeWidth = width }) drawing
