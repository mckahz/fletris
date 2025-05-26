module Tetromino exposing
    ( Letter(..)
    , OffsetTable
    , Rotation(..)
    , Tetromino
    , ccw
    , color
    , cw
    , draw
    , drawMino
    , minos
    , offsetTable
    , shift
    , stopX
    , stopY
    , toRad
    , toString
    )

import Animator as A exposing (Timeline)
import Draw exposing (Drawing)
import Element as E exposing (Color)
import Styles


type alias Tetromino =
    { letter : Letter
    , rotation : Rotation
    , x : Timeline Int
    , y : Timeline Int
    }


stopX : Tetromino -> Tetromino
stopX tetromino =
    { tetromino | x = A.go A.immediately (A.current tetromino.x) tetromino.x }


stopY : Tetromino -> Tetromino
stopY tetromino =
    { tetromino | y = A.go A.immediately (A.current tetromino.y) tetromino.y }


type Rotation
    = R0
    | R90
    | R180
    | R270


toRad : Rotation -> Float
toRad rotation =
    case rotation of
        R0 ->
            0

        R90 ->
            pi / 2

        R180 ->
            pi

        R270 ->
            -pi / 2


type Letter
    = I
    | L
    | J
    | S
    | Z
    | O
    | T


toString : Letter -> String
toString letter =
    case letter of
        Z ->
            "Z"

        L ->
            "L"

        O ->
            "O"

        S ->
            "S"

        I ->
            "I"

        J ->
            "J"

        T ->
            "T"


type alias OffsetTable =
    Rotation -> Int -> ( Int, Int )


offsetJLSTZ : OffsetTable
offsetJLSTZ rotation n =
    case ( rotation, n ) of
        ( R270, 2 ) ->
            ( 1, 0 )

        ( R270, 3 ) ->
            ( 1, -1 )

        ( R270, 4 ) ->
            ( 0, 2 )

        ( R270, 5 ) ->
            ( 1, 2 )

        ( R90, 2 ) ->
            ( -1, 0 )

        ( R90, 3 ) ->
            ( -1, -1 )

        ( R90, 4 ) ->
            ( 0, 2 )

        ( R90, 5 ) ->
            ( -1, 2 )

        ( _, _ ) ->
            ( 0, 0 )


offsetI : OffsetTable
offsetI rotation n =
    case ( rotation, n ) of
        ( R0, 2 ) ->
            ( -1, 0 )

        ( R0, 3 ) ->
            ( 2, 0 )

        ( R0, 4 ) ->
            ( -1, 0 )

        ( R0, 5 ) ->
            ( 2, 0 )

        ( R0, _ ) ->
            ( 0, 0 )

        ( R270, 2 ) ->
            ( 0, 0 )

        ( R270, 3 ) ->
            ( 0, 0 )

        ( R270, 4 ) ->
            ( 0, 1 )

        ( R270, 5 ) ->
            ( 0, -2 )

        ( R270, _ ) ->
            ( -1, 0 )

        ( R180, 2 ) ->
            ( 1, 1 )

        ( R180, 3 ) ->
            ( -2, 1 )

        ( R180, 4 ) ->
            ( 1, 0 )

        ( R180, 5 ) ->
            ( -2, 0 )

        ( R180, _ ) ->
            ( -1, 1 )

        ( R90, 2 ) ->
            ( 0, 1 )

        ( R90, 3 ) ->
            ( 0, 1 )

        ( R90, 4 ) ->
            ( 0, -1 )

        ( R90, 5 ) ->
            ( 0, 2 )

        ( R90, _ ) ->
            ( 0, 1 )


offsetO : OffsetTable
offsetO rotation _ =
    case rotation of
        R0 ->
            ( 0, 0 )

        R270 ->
            ( 0, -1 )

        R180 ->
            ( -1, -1 )

        R90 ->
            ( -1, 0 )


offsetTable : Letter -> OffsetTable
offsetTable letter =
    case letter of
        I ->
            offsetI

        O ->
            offsetO

        _ ->
            offsetJLSTZ


minos : Tetromino -> List ( Int, Int )
minos tetromino =
    tetromino.letter
        |> minosRelative
        |> List.map (shift ( A.current tetromino.x, A.current tetromino.y ))


minosRelative : Letter -> List ( Int, Int )
minosRelative letter =
    case letter of
        I ->
            [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]

        J ->
            [ ( -1, -1 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]

        L ->
            [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, -1 ) ]

        O ->
            [ ( 1, -1 ), ( 1, 0 ), ( 0, -1 ), ( 0, 0 ) ]

        S ->
            [ ( -1, 0 ), ( 0, 0 ), ( 0, -1 ), ( 1, -1 ) ]

        T ->
            [ ( -1, 0 ), ( 0, 0 ), ( 0, -1 ), ( 1, 0 ) ]

        Z ->
            [ ( -1, -1 ), ( 0, -1 ), ( 0, 0 ), ( 1, 0 ) ]


rotate : Rotation -> ( Int, Int ) -> ( Int, Int )
rotate rotation ( offx, offy ) =
    case rotation of
        R0 ->
            ( offx, offy )

        R90 ->
            ( -offy, offx )

        R180 ->
            ( -offx, -offy )

        R270 ->
            ( offy, -offx )


shift : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
shift ( x, y ) ( offx, offy ) =
    ( x + offx, y + offy )


cw : Rotation -> Rotation
cw rotation =
    case rotation of
        R0 ->
            R270

        R90 ->
            R0

        R180 ->
            R90

        R270 ->
            R180


ccw : Rotation -> Rotation
ccw rotation =
    case rotation of
        R0 ->
            R90

        R90 ->
            R180

        R180 ->
            R270

        R270 ->
            R0


color : Letter -> Color
color letter =
    case letter of
        Z ->
            Styles.zColor

        L ->
            Styles.lColor

        O ->
            Styles.oColor

        S ->
            Styles.sColor

        I ->
            Styles.iColor

        J ->
            Styles.jColor

        T ->
            Styles.tColor


drawMino : Letter -> Drawing msg
drawMino letter =
    Draw.rect (-1 / 2) (-1 / 2) 1 1
        |> Draw.fill (color letter)


draw : Tetromino -> Drawing msg
draw tetromino =
    minos tetromino
        |> List.map
            (\( mx, my ) ->
                drawMino tetromino.letter
                    |> Draw.shift (toFloat mx) (toFloat my)
                    |> Draw.shift (toFloat <| A.current tetromino.x) (toFloat <| A.current tetromino.y)
            )
        |> Draw.flatten
