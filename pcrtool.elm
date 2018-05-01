module PCRTool exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events

type RNANucleotide =
      A
    | U
    | C
    | G
    | T


toNuc : Char -> Maybe RNANucleotide

toNuc c =
    if c == 'A' || 'a' then Just A else
        if c == 'C' || 'c' then Just C else
            if c == 'U' || 'u' then Just U else
              if c == 'G' || 'g' then Just G else Nothing

aacase : (Maybe RNANucleotide, Maybe RNANucleotide, Maybe RNANucleotide) -> Character
aacase tup =
    case tup of
    (Just U, Just U, Just U) -> "F"
    (Just U, Just U, Just C) -> "F"
    (Just U, Just U, Just A) -> "L"
    (Just U, Just U, Just G) -> "L"
    (Just U, Just C, Just U) -> "S"
    (Just U, Just C, Just C) -> "S"
    (Just U, Just C, Just A) -> "S"
    (Just U, Just C, Just G) -> "S"
    (Just U, Just A, Just U) -> "Y"
    (Just U, Just A, Just C) -> "Y"
    (Just C, Just U, Just A) -> "L"
    (Just C, Just U, Just G) -> "L"
    (Just U, Just G, Just U) -> "C"
    (Just U, Just G, Just C) -> "C"
    (Just C, Just U, Just U) -> "L"
    (Just C, Just U, Just C) -> "L"
    (Just C, Just C, Just U) -> "P"
    (Just C, Just C, Just C) -> "P"
    (Just C, Just C, Just A) -> "P"
    (Just C, Just C, Just G) -> "P"
    (Just C, Just A, Just U) -> "H"
    (Just C, Just A, Just C) -> "H"
    (Just C, Just A, Just A) -> "Q"
    (Just C, Just A, Just G) -> "Q"
    (Just C, Just G, Just U) -> "R"
    (Just C, Just G, Just C) -> "R"
    (Just C, Just G, Just A) -> "R"
    (Just C, Just G, Just G) -> "R"
    (Just A, Just U, Just U) -> "I"
    (Just A, Just U, Just C) -> "I"
    (Just A, Just U, Just A) -> "I"
    (Just A, Just U, Just G) -> "M"
    (Just A, Just C, Just U) -> "T"
    (Just A, Just C, Just C) -> "T"
    (Just A, Just C, Just A) -> "T"
    (Just A, Just C, Just G) -> "T"
    (Just A, Just A, Just U) -> "N"
    (Just A, Just A, Just C) -> "N"
    (Just A, Just A, Just A) -> "K"
    (Just A, Just A, Just G) -> "K"
    (Just A, Just G, Just U) -> "S"
    (Just A, Just G, Just C) -> "S"
    (Just A, Just G, Just A) -> "R"
    (Just A, Just G, Just G) -> "R"
    (Just G, Just U, Just U) -> "V"
    (Just G, Just U, Just C) -> "V"
    (Just G, Just U, Just A) -> "V"
    (Just G, Just U, Just G) -> "V"
    (Just G, Just C, Just U) -> "A"
    (Just G, Just C, Just C) -> "A"
    (Just G, Just C, Just A) -> "A"
    (Just G, Just C, Just G) -> "A"
    (Just G, Just A, Just U) -> "D"
    (Just G, Just A, Just C) -> "D"
    (Just G, Just A, Just A) -> "E"
    (Just G, Just A, Just G) -> "E"
    (Just G, Just G, Just U) -> "G"
    (Just G, Just G, Just C) -> "G"
    (Just G, Just G, Just A) -> "G"
    (Just G, Just G, Just G) -> "G"
    (Just U, Just G, Just G) -> "W"
    (Just U, Just G, Just A) -> "Stop"
    (Just U, Just A, Just A) -> "Stop"
    (Just U, Just A, Just G) -> "Stop"
    _ -> ""


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
     , modSequence = ""}

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

