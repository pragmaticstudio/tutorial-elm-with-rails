module CountdownTimer exposing (..)

import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time exposing (Time, second)


-- Our model keeps track of the expiration time (the deadline),
-- the time remaining before the timer expires, and the timer status.
-- Tip: Time.Time is type aliased to Float


type alias Model =
    { expirationTime : Time
    , remainingTime : Time
    , status : Status
    }



-- Our timer can either be running or expired


type Status
    = Running
    | Expired



-- The timer starts in the 'Running' state with a hard-coded
-- expiration time and zero as the remaining time because
-- we don't yet know the current time (we'll fix that)


initialModel : Model
initialModel =
    { expirationTime = parseTime "Mar 06 2017 14:07:50"
    , remainingTime = 0
    , status = Running
    }



-- In the case that converting a string expiration time to a
-- Time value fails, we use a default value (0.0)


parseTime : String -> Time
parseTime string =
    Date.fromString string
        |> Result.withDefault (Date.fromTime 0)
        |> Date.toTime



-- We have a message to set the current time. The message
-- holds a Time value which is the current time (it's a float).


type Msg
    = CurrentTime Time



-- When a CurrentTime message is received, we use pattern matching
-- to extract the Time value held by the message. Then we calculate
-- the remaining time, change the status if necessary, and
-- update the corresponding model fields.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentTime now ->
            let
                remainingTime =
                    model.expirationTime - now

                status =
                    if remainingTime <= 0 then
                        Expired
                    else
                        Running
            in
            ( { model
                | remainingTime = remainingTime
                , status = status
              }
            , Cmd.none
            )



-- The view that gets rendered depends on the current model status.
-- If the timer is running, we display the remaining time. Otherwise,
-- if the timer has expired, we just display that as text.


view : Model -> Html Msg
view model =
    case model.status of
        Running ->
            div [] (viewRemainingTime model.remainingTime)

        Expired ->
            h3 [] [ text "Expired!" ]



-- Transforms time periods into a list of styled div elements
-- consisting of a time period ("days") and an amount ("02")


viewRemainingTime : Time -> List (Html msg)
viewRemainingTime t =
    List.map viewTimePeriod (timePeriods t)


viewTimePeriod : ( String, String ) -> Html msg
viewTimePeriod ( period, amount ) =
    div [ class "time-period" ]
        [ span [ class "amount" ] [ text amount ]
        , span [ class "period" ] [ text period ]
        ]



-- The remaining time is represented in milliseconds. Here we
-- calculate the remaining number of days, hours, minutes, and seconds.
-- It returns a list of tuples that looks like this, for example:
-- [ ("days", "02"), ("hours", "06"), ("minutes", "15"), ("seconds", "03")]


timePeriods : Time -> List ( String, String )
timePeriods t =
    let
        seconds =
            floor (t / 1000) % 60

        minutes =
            floor (t / 1000 / 60) % 60

        hours =
            floor (t / (1000 * 60 * 60)) % 24

        days =
            floor (t / (1000 * 60 * 60 * 24))

        addLeadingZeros n =
            String.padLeft 2 '0' (toString n)
    in
    [ days, hours, minutes, seconds ]
        |> List.map addLeadingZeros
        |> List.map2 (,) [ "days", "hours", "minutes", "seconds" ]



-- Here's the juicy part! We declare the external events our app
-- wants to listen to. In this case, we subscribe to the current
-- time. We want updates every second and we want to turn the
-- current time into CurrentTime messages that we handle in our
-- 'update' function.


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Running ->
            Time.every second CurrentTime

        Expired ->
            Sub.none


currentTime : Cmd Msg
currentTime =
    Task.perform CurrentTime Time.now



-- Html.program is initialized with a tuple that includes the
-- initial model and an initial command to run immediately
-- when the app starts.


type alias Flags =
    { deadline : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { initialModel | expirationTime = parseTime flags.deadline }, currentTime )



-- We register the subscription by setting the program's
-- 'subscriptions' field to a function that returns a subscription.


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
