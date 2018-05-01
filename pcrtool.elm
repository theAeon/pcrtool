module PCRTool exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events

type Nucleotide =
      A
    | U
    | C
    | G

aacase : Seq -> String
aacase =
    UUU -> "F"
    UUC -> "F"
    UUA -> "L"
    UUG -> "L"
    UCU -> "S"
    UCC -> "S"
    UCA -> "S"
    UCG -> "S"
    UAU -> "Y"
    UAC -> "Y"
    CUA -> "L"
    CUG -> "L"
    UGU -> "C"
    UGC -> "C"
    CUU -> "L"
    CUC -> "L"
    CCU -> "P"
    CCC -> "P"
    CCA -> "P"
    CCG -> "P"
    CAU -> "H"
    CAC -> "H"
    CAA -> "Q"
    CAG -> "Q"
    CGU -> "R"
    CGC -> "R"
    CGA -> "R"
    CGG -> "R"
    AUU -> "I"
    AUC -> "I"
    AUA -> "I"
    AUG -> "M"
    ACU -> "T"
    ACC -> "T"
    ACA -> "T"
    ACG -> "T"
    AAU -> "N"
    AAC -> "N"
    AAA -> "K"
    AAG -> "K"
    AGU -> "S"
    AGC -> "S"
    AGA -> "R"
    AGG -> "R"
    GUU -> "V"
    GUC -> "V"
    GUA -> "V"
    GUG -> "V"
    GCU -> "A"
    GCC -> "A"
    GCA -> "A"
    GCG -> "A"
    GAU -> "D"
    GAC -> "D"
    GAA -> "E"
    GAG -> "E"
    GGU -> "G"
    GGC -> "G"
    GGA -> "G"
    GGG -> "G"
    UGG -> "W"
    UGA -> "Stop"
    UAA -> "Stop"
    UAG -> "Stop"
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

