module Tetromino exposing
    ( Letter(..)
    , OffsetTable
    , Rotation(..)
    , Tetromino
    , ccw
    , color
    , cw
    , down
    , left
    , minos
    , offsetTable
    , overlaps
    , right
    , toString
    )

import Element as E exposing (Color)
import Styles


type Rotation
    = R0
    | R90
    | R180
    | R270


type alias Tetromino =
    { letter : Letter
    , rotation : Rotation
    , x : Int
    , y : Int
    }


overlaps : Int -> Int -> Tetromino -> Bool
overlaps x y tetromino =
    minos tetromino
        |> List.any (\( px, py ) -> px == x && py == y)


type Letter
    = I
    | L
    | J
    | S
    | Z
    | O
    | T


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
    let
        init =
            case tetromino.letter of
                I ->
                    [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]

                J ->
                    [ ( -1, 1 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ) ]

                L ->
                    [ ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( 1, 1 ) ]

                O ->
                    [ ( 1, 1 ), ( 1, 0 ), ( 0, 1 ), ( 0, 0 ) ]

                S ->
                    [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 1 ) ]

                T ->
                    [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 0 ) ]

                Z ->
                    [ ( -1, 1 ), ( 0, 1 ), ( 0, 0 ), ( 1, 0 ) ]

        rotate ( offx, offy ) =
            case tetromino.rotation of
                R0 ->
                    ( offx, offy )

                R90 ->
                    ( -offy, offx )

                R180 ->
                    ( -offx, -offy )

                R270 ->
                    ( offy, -offx )

        shift ( offx, offy ) =
            ( tetromino.x + offx, tetromino.y + offy )
    in
    init
        |> List.map rotate
        |> List.map shift


down : Tetromino -> Tetromino
down tetromino =
    { tetromino | y = tetromino.y - 1 }


left : Tetromino -> Tetromino
left tetromino =
    { tetromino | x = tetromino.x - 1 }


right : Tetromino -> Tetromino
right tetromino =
    { tetromino | x = tetromino.x + 1 }


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
