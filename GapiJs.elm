module GapiJs exposing (..)

import JavaScript as Js


load : String -> Js.Func () Js.VoidType -> Js.Void
load components callback =
    Js.void <|
        "gapi.load("
            ++ components
            ++ ","
            ++ callback.str
            ++ ")"


test : String -> Js.Func ( Js.Int, Js.Str ) Js.Date -> Js.Void
test fromElm callback =
    Js.void <|
        "gapi.load("
            ++ fromElm
            ++ ","
            ++ callback.str
            ++ ")"


testThis : Js.Void
testThis =
    test "hello"
        (Js.func
            (Js.args ( Js.str "hi", Js.int "7" ))
            (Js.block
                [ Js.str "blah" |> Js.stmt
                , Js.bool "hah" |> Js.stmt
                ]
                (Js.return
                    (Js.date "new Date()")
                )
            )
        )
