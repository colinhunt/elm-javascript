module Main exposing (..)

import Html exposing (Html, div, text)
import JavaScript as Js exposing (var, chainable)
import GapiJs


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { count : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model 1, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { count } =
    div []
        [ text <| Debug.log "msg" javaScript
        ]


type alias Console =
    { log : Js.Str -> Js.Void }


console : Console
console =
    { log =
        (\{ str } ->
            Js.void <| String.concat [ "console.log(", str, ")" ]
        )
    }


type alias Date =
    { ctorInt : Js.Int -> Js.Date
    , now : Js.Int
    , getDay : Js.Date -> Js.Str
    , setDate : Js.Int -> Js.Date -> Js.Date
    , toLocaleDateString : Js.Date -> Js.Str
    }


date : Date
date =
    { ctorInt = (\{ str } -> Js.date <| "new Date(" ++ str ++ ")")
    , now = Js.int "Date.now()"
    , getDay = (\{ str } -> Js.str <| str ++ ".getDay()")
    , setDate = (\v d -> Js.date <| chainable d (".setDate(" ++ v.str ++ ")"))
    , toLocaleDateString = \{ str } -> Js.str <| str ++ ".toLocaleDateString()"
    }


javaScript : String
javaScript =
    let
        ( d, makeDate ) =
            --var "d" (date.ctorInt (date.getDay date.now)) -- Compiler error yay!
            var "d" (date.ctorInt date.now)
    in
        Js.block
            [ makeDate |> Js.stmt
            , (d
                |> date.setDate (Js.int "7")
                |> date.toLocaleDateString
                |> console.log
              )
                |> Js.stmt
            ]
            (Js.return
                GapiJs.testThis
            )
            |> .str


quote : String -> String
quote str =
    "'" ++ str ++ "'"
