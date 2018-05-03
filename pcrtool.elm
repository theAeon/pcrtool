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
      DNA
    | DNT
    | DNC
    | DNG

transcribe : Maybe DNANucleotide -> Maybe RNANucleotide
transcribe nuc =
    case nuc of
    Just DNA -> Just U
    Just DNT -> Just A
    Just DNC -> Just G
    Just DNG -> Just C
    Nothing -> Nothing


toNuc : Char -> Maybe DNANucleotide

toNuc c =
    if c == 'A' || c == 'a' then Just DNA else
        if c == 'C' || c == 'c' then Just DNC else
            if c == 'T' || c == 't' then Just DNT else
              if c == 'G' || c == 'g' then Just DNG else Nothing

codonMake : List (Maybe RNANucleotide) -> List (RNANucleotide,RNANucleotide, RNANucleotide)
codonMake list =
    case list of
    (Just x)::(Just y)::(Just z)::t -> (x, y, z) :: codonMake t
    Nothing::t -> codonMake t
    (f)::Nothing::t -> codonMake (f :: t)
    (f)::(s)::Nothing::t -> codonMake (f:: s::t)
    _ -> []



aacase : (RNANucleotide, RNANucleotide, RNANucleotide) -> String
aacase tup =
    case tup of
    (U, _, _) -> 
        case tup of
        (_, U, _) -> case tup of
                    (_, _, U) -> "F"
                    (_, _, C) -> "F"
                    (_, _, A) -> "L"
                    (_, _, G) -> "L"
        (_, C, _) -> "S"
        (_, A, _) -> case tup of
                    (_, _, U) -> "Y"
                    (_, _, C) -> "Y"
                    (_, _, A) -> "Stop"
                    (_, _, G) -> "Stop"
        (_, G, _) -> case tup of
                    (_, _, U) -> "C"
                    (_, _, C) -> "C"
                    (_, _, A) -> "Stop"
                    (_, _, G) -> "W"
    (C, _, _) -> 
        case tup of
        (_, U, _) -> "L"
        (_, C, _) -> "P"
        (_, A, _) -> case tup of
                    (_, _, U) -> "H"
                    (_, _, C) -> "H"
                    (_, _, A) -> "Q"
                    (_, _, G) -> "Q"
        (_, G, _) -> "R"

    (A, _, _) -> 
        case tup of
        (_, U, _) -> case tup of
                    (_, _, U) -> "I"
                    (_, _, C) -> "I"
                    (_, _, A) -> "I"
                    (_, _, G) -> "M"
        (_, C, _) -> "T"
        (_, A, _) -> case tup of
                    (_, _, U) -> "N"
                    (_, _, C) -> "N"
                    (_, _, A) -> "K"
                    (_, _, G) -> "K"
        (_, G, _) -> case tup of
                    (_, _, U) -> "S"
                    (_, _, C) -> "S"
                    (_, _, A) -> "R"
                    (_, _, G) -> "R"
    (G, _, _) -> 
        case tup of
        (_, U, _) -> "V"
        (_, C, _) -> "A"
        (_, A, _) -> case tup of
                    (_, _, U) -> "D"
                    (_, _, C) -> "D"
                    (_, _, A) -> "E"
                    (_, _, G) -> "E"
        (_, G, _) -> "G"
        
type alias AppState =
    { sequence : String
    , forward: String
    , reverse: String
    , aminoacid: String
    , modSequence : String}


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
        , (Html.textarea [ Events.onInput (\s -> UpdateSeq s), Attrs.placeholder "target sequence", Attrs.style [("resize", "none"), ("margin", "auto auto 1em 2em")]] [])
        , (Html.textarea [ Events.onInput (\s -> UpdateFor s), Attrs.placeholder "forward sequence", Attrs.style [("resize", "none"), ("margin", "auto auto 1em 2em")]] [])
        , (Html.textarea [ Events.onInput (\s -> UpdateRev s), Attrs.placeholder "reverse sequence", Attrs.style [("resize", "none"), ("margin", "auto auto 1em 2em")]] [])
        , Html.div [] [Html.p [] [], Html.p [] []]]


update : AppEvent -> AppState -> AppState
update e s =
    case e of
        UpdateSeq str -> {s | sequence = str}
        UpdateFor str -> {s | forward = str}
        UpdateRev str -> {s | reverse = str}
        ReCalculate ->  s


seqToNuc : List String -> List Maybe DNANucleotide

seqToNuc seq =
    List.map (toNuc) String.toList seq

forwardBind : List Maybe DNANucleotide -> List Maybe DNANucleotide -> Int
forwardBind seq for =
    case (seq, for) of
        (hs::ts, hf::tf) -> if hs /= hf then forwardBind ts tf else
                                hs :: (forwardBind ts tf)
 












main =
    view initialState

