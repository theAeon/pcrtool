module PCRTool exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events


type alias AppState =


initialState : AppState
initialState =


type AppEvent
    = 


view : AppState -> Html.Html AppEvent
view state =
    Html.div []
        [ Html.h1 [("text-align", "center")] [ Html.text "PCRTool" ]
        , (Html.input [ Events.onInput (\s -> EditTask index s), Attrs.value task ] [])
        ]


main =
    view initialState

