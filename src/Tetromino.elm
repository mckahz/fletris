module Tetromino exposing
    ( Dir(..)
    , Motion(..)
    , OffsetTable
    , Tetromino
    , arrEvents
    , dasEvents
    , decode
    , dirToInt
    , encode
    , getX
    , getY
    , minos
    , move
    , offsetTable
    , setX
    , setY
    , shift
    )

import Animate as A exposing (Timeline)
import Json.Decode as Decode
import Json.Encode as Encode
import Settings
import Tetromino.Letter as Letter exposing (Letter(..))
import Tetromino.Orientation as Orientation exposing (Orientation(..))


encode : Tetromino -> Encode.Value
encode tetromino =
    Encode.object
        [ ( "letter", Letter.encode tetromino.letter )
        , ( "orientation", Orientation.encode tetromino.orientation )
        , ( "x", Encode.int <| .position <| A.current tetromino.x )
        , ( "y", Encode.int <| A.current tetromino.y )
        ]


decode : Decode.Decoder Tetromino
decode =
    Decode.map4 Tetromino
        (Decode.field "letter" Letter.decode)
        (Decode.field "orientation" Orientation.decode)
        (Decode.field "x" (Decode.int |> Decode.map (\x -> A.init { position = x, motion = Nothing })))
        (Decode.field "y" (Decode.int |> Decode.map A.init))


type alias Tetromino =
    { letter : Letter
    , orientation : Orientation
    , x : Timeline { position : Int, motion : Maybe ( Motion, Dir ) }
    , y : Timeline Int
    }


type Motion
    = Das
    | Arr


type Dir
    = Left
    | Right


dirToInt : Dir -> Int
dirToInt dir =
    case dir of
        Right ->
            1

        Left ->
            -1


move : Settings.Handling -> Motion -> Dir -> Tetromino -> Tetromino
move handling motion dir tetromino =
    let
        events =
            case motion of
                Arr ->
                    arrEvents

                Das ->
                    dasEvents
    in
    { tetromino
        | x =
            tetromino.x
                |> A.interrupt (events handling (getX tetromino) dir)
    }


dasEvents : Settings.Handling -> Int -> Dir -> List (A.Step { position : Int, motion : Maybe ( Motion, Dir ) })
dasEvents handling x dir =
    [ A.go A.immediately { position = x + dirToInt dir, motion = Just <| ( Das, dir ) }
    , A.wait (A.millis handling.das)
    ]
        ++ arrEvents handling (x + dirToInt dir) dir


arrEvents : Settings.Handling -> Int -> Dir -> List (A.Step { position : Int, motion : Maybe ( Motion, Dir ) })
arrEvents handling x dir =
    let
        offsetToEvent off =
            A.go A.immediately { position = x + off * dirToInt dir, motion = Just <| ( Arr, dir ) }

        maxOffset =
            10
    in
    case handling.arr of
        Settings.ArrInfinite ->
            [ offsetToEvent maxOffset ]

        Settings.ArrGradual arr ->
            List.range 1 maxOffset
                |> List.map offsetToEvent
                |> List.intersperse (A.wait (A.millis arr))


getX : Tetromino -> Int
getX tetromino =
    tetromino.x
        |> A.current
        |> .position


getY : Tetromino -> Int
getY tetromino =
    tetromino.y
        |> A.current


setX : Int -> Tetromino -> Tetromino
setX x tetromino =
    { tetromino | x = A.init { position = x, motion = Nothing } }


setY : Int -> Tetromino -> Tetromino
setY y tetromino =
    { tetromino | y = A.init y }



-- Hard coded SRS offset tables


type alias OffsetTable =
    Orientation -> Int -> ( Int, Int )


offsetJLSTZ : OffsetTable
offsetJLSTZ orientation n =
    case ( orientation, n ) of
        ( East, 2 ) ->
            ( 1, 0 )

        ( East, 3 ) ->
            ( 1, 1 )

        ( East, 4 ) ->
            ( 0, -2 )

        ( East, 5 ) ->
            ( 1, -2 )

        ( West, 2 ) ->
            ( -1, 0 )

        ( West, 3 ) ->
            ( -1, 1 )

        ( West, 4 ) ->
            ( 0, -2 )

        ( West, 5 ) ->
            ( -1, -2 )

        ( _, _ ) ->
            ( 0, 0 )


offsetI : OffsetTable
offsetI orientation n =
    case ( orientation, n ) of
        ( North, 2 ) ->
            ( -1, 0 )

        ( North, 3 ) ->
            ( 2, 0 )

        ( North, 4 ) ->
            ( -1, 0 )

        ( North, 5 ) ->
            ( 2, 0 )

        ( North, _ ) ->
            ( 0, 0 )

        ( East, 2 ) ->
            ( 0, 0 )

        ( East, 3 ) ->
            ( 0, 0 )

        ( East, 4 ) ->
            ( 0, -1 )

        ( East, 5 ) ->
            ( 0, -2 )

        ( East, _ ) ->
            ( -1, 0 )

        ( South, 2 ) ->
            ( 1, -1 )

        ( South, 3 ) ->
            ( -2, -1 )

        ( South, 4 ) ->
            ( 1, 0 )

        ( South, 5 ) ->
            ( -2, 0 )

        ( South, _ ) ->
            ( -1, -1 )

        ( West, 2 ) ->
            ( 0, -1 )

        ( West, 3 ) ->
            ( 0, -1 )

        ( West, 4 ) ->
            ( 0, 1 )

        ( West, 5 ) ->
            ( 0, -2 )

        ( West, _ ) ->
            ( 0, -1 )


offsetO : OffsetTable
offsetO orientation _ =
    case orientation of
        North ->
            ( 0, 0 )

        East ->
            ( 0, 1 )

        South ->
            ( -1, 1 )

        West ->
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
        |> List.map (rotate tetromino.orientation)
        |> List.map (shift ( .position <| A.current tetromino.x, A.current tetromino.y ))


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


rotate : Orientation -> ( Int, Int ) -> ( Int, Int )
rotate orientation ( x, y ) =
    case orientation of
        North ->
            ( x, y )

        East ->
            ( -y, x )

        South ->
            ( -x, -y )

        West ->
            ( y, -x )


shift : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
shift ( x, y ) ( offx, offy ) =
    ( x + offx, y + offy )
