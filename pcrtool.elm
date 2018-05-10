module PCRTool exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events


type RNANucleotide
    = A
    | U
    | C
    | G


type DNANucleotide
    = DNA
    | DNT
    | DNC
    | DNG


transcribe : DNANucleotide -> RNANucleotide
transcribe nuc =
    case nuc of
        DNA ->
            U

        DNT ->
            A

        DNC ->
            G

        DNG ->
            C


translate : DNANucleotide -> DNANucleotide
translate nuc =
    case nuc of
        DNA ->
            DNT

        DNT ->
            DNA

        DNC ->
            DNG

        DNG ->
            DNC


toNuc : Char -> Maybe DNANucleotide
toNuc c =
    if c == 'A' || c == 'a' then
        Just DNA
    else if c == 'C' || c == 'c' then
        Just DNC
    else if c == 'T' || c == 't' then
        Just DNT
    else if c == 'G' || c == 'g' then
        Just DNG
    else
        Nothing


toStr : DNANucleotide -> String
toStr nuc =
    case nuc of
        DNA ->
            "A"

        DNC ->
            "C"

        DNT ->
            "T"

        DNG ->
            "G"


codonMake : List RNANucleotide -> List ( RNANucleotide, RNANucleotide, RNANucleotide )
codonMake list =
    case list of
        x :: y :: z :: t ->
            ( x, y, z ) :: codonMake t

        _ ->
            []


aacase : ( RNANucleotide, RNANucleotide, RNANucleotide ) -> String
aacase tup =
    case tup of
        ( U, _, _ ) ->
            case tup of
                ( _, U, _ ) ->
                    case tup of
                        ( _, _, U ) ->
                            "F "

                        ( _, _, C ) ->
                            "F "

                        ( _, _, A ) ->
                            "L "

                        ( _, _, G ) ->
                            "L "

                ( _, C, _ ) ->
                    "S "

                ( _, A, _ ) ->
                    case tup of
                        ( _, _, U ) ->
                            "Y "

                        ( _, _, C ) ->
                            "Y "

                        ( _, _, A ) ->
                            "Stop "

                        ( _, _, G ) ->
                            "Stop "

                ( _, G, _ ) ->
                    case tup of
                        ( _, _, U ) ->
                            "C "

                        ( _, _, C ) ->
                            "C "

                        ( _, _, A ) ->
                            "Stop "

                        ( _, _, G ) ->
                            "W "

        ( C, _, _ ) ->
            case tup of
                ( _, U, _ ) ->
                    "L "

                ( _, C, _ ) ->
                    "P "

                ( _, A, _ ) ->
                    case tup of
                        ( _, _, U ) ->
                            "H "

                        ( _, _, C ) ->
                            "H "

                        ( _, _, A ) ->
                            "Q "

                        ( _, _, G ) ->
                            "Q "

                ( _, G, _ ) ->
                    "R "

        ( A, _, _ ) ->
            case tup of
                ( _, U, _ ) ->
                    case tup of
                        ( _, _, U ) ->
                            "I "

                        ( _, _, C ) ->
                            "I "

                        ( _, _, A ) ->
                            "I "

                        ( _, _, G ) ->
                            "M "

                ( _, C, _ ) ->
                    "T "

                ( _, A, _ ) ->
                    case tup of
                        ( _, _, U ) ->
                            "N "

                        ( _, _, C ) ->
                            "N "

                        ( _, _, A ) ->
                            "K "

                        ( _, _, G ) ->
                            "K "

                ( _, G, _ ) ->
                    case tup of
                        ( _, _, U ) ->
                            "S "

                        ( _, _, C ) ->
                            "S "

                        ( _, _, A ) ->
                            "R "

                        ( _, _, G ) ->
                            "R "

        ( G, _, _ ) ->
            case tup of
                ( _, U, _ ) ->
                    "V "

                ( _, C, _ ) ->
                    "A "

                ( _, A, _ ) ->
                    case tup of
                        ( _, _, U ) ->
                            "D "

                        ( _, _, C ) ->
                            "D "

                        ( _, _, A ) ->
                            "E "

                        ( _, _, G ) ->
                            "E "

                ( _, G, _ ) ->
                    "G "


type alias AppState =
    { sequence : String
    , forward : String
    , reverse : String
    , aminoacid : String
    , modSequence : String
    }


initialState : AppState
initialState =
    { sequence = ""
    , forward = ""
    , reverse = ""
    , aminoacid = ""
    , modSequence = ""
    }


type AppEvent
    = UpdateSeq String
    | UpdateFor String
    | UpdateRev String
    | ReCalculate


view : AppState -> Html.Html AppEvent
view state =
    Html.div []
        [ (Html.h1 [ Attrs.style [ ( "text-align", "center" ) ] ] [ Html.text "PCRTool" ])
        , (Html.textarea [ Events.onInput (\s -> UpdateSeq s), Attrs.placeholder "target sequence", Attrs.style [ ( "resize", "none" ), ( "margin", "auto auto 1em 2em" ) ] ] [])
        , (Html.textarea [ Events.onInput (\s -> UpdateFor s), Attrs.placeholder "forward primer", Attrs.style [ ( "resize", "none" ), ( "margin", "auto auto 1em 2em" ) ] ] [])
        , (Html.textarea [ Events.onInput (\s -> UpdateRev s), Attrs.placeholder "reverse primer", Attrs.style [ ( "resize", "none" ), ( "margin", "auto auto 1em 2em" ) ] ] [])
        , (Html.div [ Attrs.style [ ( "display", "flex" ), ( "flex-direction", "column" ) ] ]
            [ (Html.button [ Attrs.style [ ( "margin", "auto auto 1em 2em" ) ], Events.onClick ReCalculate ] [ Html.text "Run PCR!" ])
            , (Html.textarea [ Attrs.style [ ( "resize", "none" ), ( "margin", "auto auto 1em 2em" ), ( "width", "41.8%" ) ] ] [ Html.text state.modSequence ])
            , (Html.textarea [ Attrs.style [ ( "resize", "none" ), ( "margin", "auto auto 1em 2em" ), ( "width", "41.8%" ) ] ] [ Html.text state.aminoacid ])
            ]
          )
        , Html.div [] [ Html.p [] [], Html.p [] [] ]
        ]


update : AppEvent -> AppState -> AppState
update e s =
    case e of
        UpdateSeq str ->
            { s | sequence = str }

        UpdateFor str ->
            { s | forward = str }

        UpdateRev str ->
            { s | reverse = str }

        ReCalculate ->
            let
                modSeq =
                    (amplified (forwardBind (seqToNuc s.sequence) (seqToNuc s.forward) (seqToNuc s.forward)) (reverseBind s.sequence (seqToNuc s.reverse)) s.sequence) ++ (appendRev s.reverse)
            in
                { s | modSequence = modSeq, aminoacid = List.foldr (++) "" (List.map aacase (codonMake (List.map transcribe (seqToNuc modSeq)))) }


seqToNuc : String -> List DNANucleotide
seqToNuc seq =
    List.filterMap toNuc (String.toList seq)


foldNuc : List String -> String
foldNuc list =
    List.foldr (++) "" list


forwardBind : List DNANucleotide -> List DNANucleotide -> List DNANucleotide -> Int
forwardBind seq for ori =
    case ( seq, for, ori ) of
        ( hs :: ts, _, [] ) ->
            1 + forwardBind ts for ori

        ( hs :: ts, hf :: tf, _ ) ->
            if hs /= hf then
                1 + forwardBind ts ori ori
            else
                (forwardBind ts tf ori)

        ( _, _, _ ) ->
            (0)


appendRev : String -> String
appendRev string =
    foldNuc (List.map (toStr) (List.map translate (seqToNuc (String.reverse string))))


reverseBind : String -> List DNANucleotide -> Int
reverseBind seq rev =
    let
        reversedprimer =
            (List.reverse rev)

        reversedseq =
            (String.reverse seq)
    in
        forwardBind (seqToNuc reversedseq) (List.map translate reversedprimer) (List.map translate reversedprimer)


amplified : Int -> Int -> String -> String
amplified i1 i2 seq =
    String.dropLeft i1 (String.dropRight i2 (foldNuc (List.map toStr (seqToNuc seq))))


main =
    Html.beginnerProgram
        { view = view
        , update = update
        , model = initialState
        }
