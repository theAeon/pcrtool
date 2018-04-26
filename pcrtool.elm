module PCRTool exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events


type alias AppState =
    { sequence : String
    , forward: String
    , reverse: String}


initialState : AppState
initialState =
    {  sequence = ""
     , forward = ""
     , reverse = ""}

type AppEvent
    = UpdateSeq String |
      UpdateFor String |
      UpdateRev String


view : AppState -> Html.Html AppEvent
view state =
    Html.div []
        [ (Html.h1 [Attrs.style [("text-align", "center")]] [ Html.text "PCRTool" ])
        , (Html.textarea [ Events.onInput (\s -> UpdateSeq s), Attrs.placeholder "target sequence"] [])
        , (Html.textarea [ Events.onInput (\s -> UpdateFor s), Attrs.placeholder "forward sequence"] [])
        , (Html.textarea [ Events.onInput (\s -> UpdateRev s), Attrs.placeholder "reverse sequence"] [])
        ]


main =
    view initialState

