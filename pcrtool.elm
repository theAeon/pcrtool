module PCRTool exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events


type alias AppState =
    { sequence : String
    , forward: String
    , reverse: String
    , aminoacid: String
    , modsequence : String}


initialState : AppState
initialState =
    {  sequence = ""
     , forward = ""
     , reverse = ""
     , aminoacid = ""
     , ModSequence = ""}
type AppEvent
    = UpdateSeq String |
      UpdateFor String |
      UpdateRev String |
      ReCalculate


view : AppState -> Html.Html AppEvent
view state =
    Html.div []
        [ (Html.h1 [Attrs.style [("text-align", "center")]] [ Html.text "PCRTool" ])
        , (Html.textarea [ Events.onInput (\s -> UpdateSeq s), Attrs.placeholder "target sequence", Attrs.style [("resize", "none")]] [])
        , (Html.textarea [ Events.onInput (\s -> UpdateFor s), Attrs.placeholder "forward sequence", Attrs.style [("resize", "none")]] [])
        , (Html.textarea [ Events.onInput (\s -> UpdateRev s), Attrs.placeholder "reverse sequence", Attrs.style [("resize", "none")]] [])
        , Html.div [] [Html.p [] [], Html.p [] []]]


update : AppEvent -> AppState -> AppState
update e s =
    case e of
        UpdateSeq str -> {s | sequence = str}
        UpdateFor str -> {s | forward = str}
        UpdateRev str -> {s | reverse = str}
        ReCalculate -> {s | modsequence = , aminoacid = }


main =
    view initialState

