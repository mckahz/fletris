module Animate exposing
    ( Step
    , Timeline
    , current
    , detour
      --, finished
    , go
    , immediately
    , init
    , interrupt
    , map
    , millis
    , stop
    , update
    , wait
    )

import List.Extra as List
import Time


type Timeline state
    = Timeline
        { now : Time.Posix
        , init : state
        , events : List (Event state)
        }


type alias Event state =
    { state : state
    , time : Time.Posix
    , duration : Duration
    }


map : (state1 -> state2) -> Timeline state1 -> Timeline state2
map f (Timeline timeline) =
    Timeline
        { now = timeline.now
        , init = f timeline.init
        , events =
            timeline.events
                |> List.map (\event -> { state = f event.state, time = event.time, duration = event.duration })
        }


init : state -> Timeline state
init state =
    Timeline
        { now = Time.millisToPosix 0
        , init = state
        , events = []
        }


update : Time.Posix -> Timeline state -> Timeline state
update time (Timeline timeline) =
    Timeline
        { timeline
            | now = time
        }


current : Timeline state -> state
current (Timeline timeline) =
    timeline.events
        |> List.takeWhile (\event -> Time.posixToMillis timeline.now > Time.posixToMillis event.time)
        |> List.last
        |> Maybe.map .state
        |> Maybe.withDefault timeline.init



-- finished : Timeline state -> Bool
-- finished (Timeline timeline) =
--     List.all (\event -> Time.posixToMillis event.time + round (toMillis event.duration) <= Time.posixToMillis timeline.now) timeline.events


interrupt : List (Step state) -> Timeline state -> Timeline state
interrupt steps (Timeline timeline) =
    let
        addStep step ( events, now ) =
            case step of
                Wait duration ->
                    ( events, now + round (toMillis duration) )

                Transition duration state ->
                    ( events ++ [ { state = state, time = Time.millisToPosix now, duration = duration } ]
                    , now + round (toMillis duration)
                    )

        truncatedEvents =
            timeline.events
                |> List.takeWhile (\event -> Time.posixToMillis event.time <= Time.posixToMillis timeline.now)
    in
    Timeline
        { timeline
            | events =
                steps
                    |> List.foldl addStep
                        ( truncatedEvents, Time.posixToMillis timeline.now )
                    |> Tuple.first
        }


detour : List (Step state) -> Timeline state -> Timeline state
detour steps (Timeline timeline) =
    let
        (Timeline withoutFuture) =
            interrupt steps (Timeline timeline)

        finalTime =
            withoutFuture.events
                |> List.last
                |> Maybe.map (\event -> Time.posixToMillis event.time + round (toMillis event.duration))
                |> Maybe.withDefault (Time.posixToMillis timeline.now)

        dt =
            finalTime - Time.posixToMillis timeline.now

        future =
            timeline.events
                |> List.dropWhile (\event -> Time.posixToMillis event.time <= Time.posixToMillis timeline.now)
                |> List.map (\event -> { event | time = Time.millisToPosix (Time.posixToMillis event.time + dt) })
    in
    Timeline
        { withoutFuture
            | events =
                withoutFuture.events ++ future
        }


stop : Timeline state -> Timeline state
stop timeline =
    interrupt [] timeline


type Duration
    = Instant
    | Millis Float


millis : Float -> Duration
millis =
    Millis


toMillis : Duration -> Float
toMillis duration =
    case duration of
        Instant ->
            0

        Millis m ->
            m


immediately : Duration
immediately =
    Instant


type Step state
    = Wait Duration
    | Transition Duration state


wait : Duration -> Step state
wait =
    Wait


go : Duration -> state -> Step state
go =
    Transition
