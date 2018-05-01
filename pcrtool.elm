module PCRTool exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events

type RNANucleotide =
      A
    | U
    | C
    | G

type DNANucleotide =
      A
    | T
    | C
    | G

transcribe : Maybe DNANucleotide -> Maybe RNANucleotide
transcribe nuc =
    case nuc of
    Just A -> Just U
    Just T -> Just A
    Just C -> Just G
    Just G -> Just C
    Nothing -> Nothing


toNuc : Char -> Maybe DNANucleotide

toNuc c =
    if c == 'A' || 'a' then Just A else
        if c == 'C' || 'c' then Just C else
            if c == 'T' || 't' then Just T else
              if c == 'G' || 'g' then Just G else Nothing

codonMake : List Maybe RNANucleotide -> List (RNANucleotide,RNANucleotide, RNANucleotide)

codonMake list =
    case list of
    Just x::Just y::Just z::t -> (x, y, z) :: (codonMake t)
    Nothing::t -> codonMake t
    f::Nothing::t -> f :: codonMake t
    f::s::Nothing::t -> f :: s:: codonMake t
    _ -> []



aacase : (RNANucleotide, RNANucleotide, RNANucleotide) -> String
aacase tup =
    case tup of
    (U, U, U) -> "F"
    (U, U, C) -> "F"
    (U, U, A) -> "L"
    (U, U, G) -> "L"
    (U, C, U) -> "S"
    (U, C, C) -> "S"
    (U, C, A) -> "S"
    (U, C, G) -> "S"
    (U, A, U) -> "Y"
    (U, A, C) -> "Y"
    (C, U, A) -> "L"
    (C, U, G) -> "L"
    (U, G, U) -> "C"
    (U, G, C) -> "C"
    (C, U, U) -> "L"
    (C, U, C) -> "L"
    (C, C, U) -> "P"
    (C, C, C) -> "P"
    (C, C, A) -> "P"
    (C, C, G) -> "P"
    (C, A, U) -> "H"
    (C, A, C) -> "H"
    (C, A, A) -> "Q"
    (C, A, G) -> "Q"
    (C, G, U) -> "R"
    (C, G, C) -> "R"
    (C, G, A) -> "R"
    (C, G, G) -> "R"
    (A, U, U) -> "I"
    (A, U, C) -> "I"
    (A, U, A) -> "I"
    (A, U, G) -> "M"
    (A, C, U) -> "T"
    (A, C, C) -> "T"
    (A, C, A) -> "T"
    (A, C, G) -> "T"
    (A, A, U) -> "N"
    (A, A, C) -> "N"
    (A, A, A) -> "K"
    (A, A, G) -> "K"
    (A, G, U) -> "S"
    (A, G, C) -> "S"
    (A, G, A) -> "R"
    (A, G, G) -> "R"
    (G, U, U) -> "V"
    (G, U, C) -> "V"
    (G, U, A) -> "V"
    (G, U, G) -> "V"
    (G, C, U) -> "A"
    (G, C, C) -> "A"
    (G, C, A) -> "A"
    (G, C, G) -> "A"
    (G, A, U) -> "D"
    (G, A, C) -> "D"
    (G, A, A) -> "E"
    (G, A, G) -> "E"
    (G, G, U) -> "G"
    (G, G, C) -> "G"
    (G, G, A) -> "G"
    (G, G, G) -> "G"
    (U, G, G) -> "W"
    (U, G, A) -> "Stop"
    (U, A, A) -> "Stop"
    (U, A, G) -> "Stop"


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
        ReCalculate ->   {s | modsequence = 

            


        ,
         aminoacid = }


forwardBind : 
forwardBind state =
    let
        sequence = List.map toNuc String.toList(state.sequence)
        , forward = List.map toNuc String.toList(state.forward)

    in
        case (sequence, forward) of
            (hs::ts, hf::tf) -> if hs != ht then 













main =
    view initialState

