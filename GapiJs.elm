module GapiJs exposing (..)

import JavaScript as Js exposing (funcToString)


load : String -> Js.Func () Js.VoidType -> Js.Void
load components callback =
    Js.void <|
        "gapi.load("
            ++ components
            ++ ","
            ++ funcToString callback
            ++ ")"


testThis : Js.Void
testThis =
    load "hello" (Js.func () Js.VoidType (Js.block "blah"))
