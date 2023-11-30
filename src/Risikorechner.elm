module Risikorechner exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, classList, for, id, placeholder, type_, value)
import Html.Events exposing (onCheck, onInput)



-- MODEL


init : Model
init =
    { itemValue = 5000
    , instances = OneInstance
    , vatCustomer = True
    , vatOpponent = True
    , additionalCosts = 0
    , probability = 50
    }


type alias Model =
    { itemValue : Int
    , instances : Instances
    , vatCustomer : VAT
    , vatOpponent : VAT
    , additionalCosts : Int
    , probability : Int
    }


type Instances
    = OneInstance
    | TwoInstances


strToInstances : String -> Instances
strToInstances s =
    case s of
        "OneInstance" ->
            OneInstance

        "TwoInstances" ->
            TwoInstances

        _ ->
            OneInstance


type alias VAT =
    Bool



-- UPDATE


type Msg
    = ItemValueForm Int
    | AdditionalCostsForm Int
    | InstancesForm Instances
    | VatCustomerForm VAT
    | VatOpponentForm VAT
    | ProbabilityForm Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ItemValueForm newItemValue ->
            { model | itemValue = newItemValue }

        AdditionalCostsForm newAdditionalCosts ->
            { model | additionalCosts = newAdditionalCosts }

        InstancesForm newInstances ->
            { model | instances = newInstances }

        VatCustomerForm newVatCustomer ->
            { model | vatCustomer = newVatCustomer }

        VatOpponentForm newVatOpponent ->
            { model | vatOpponent = newVatOpponent }

        ProbabilityForm newProbability ->
            { model | probability = newProbability }



-- VIEW


view : Model -> Html Msg
view model =
    div [ classes "container p-3 pb-5" ]
        [ main_ []
            [ h1 [ class "mb-3" ] [ text "TextBauStein Risikorechner" ]
            , div [ class "mb-5" ]
                [ h2 [ class "mb-3" ] [ text "Eingaben" ]
                , modelInput model
                ]
            , div [ class "mb-5" ]
                [ h2 [ class "mb-3" ] [ text "Ergebnis" ]
                , result model
                ]
            ]
        ]


modelInput : Model -> Html Msg
modelInput model =
    let
        itemValueLabel =
            "Streitwert"

        additionalCostsLabel =
            "Sonstige Gerichtskosten, z. B. für Gutachten"

        instancesLabel =
            "Instanzen"

        vatCustomerLabel =
            "Umsatzsteuer (19 %) Mandant"

        vatOpponentLabel =
            "Umsatzsteuer (19 %) Gegner"

        probabilityLabel =
            "Erfolgswahrscheinlichkeit"
    in
    div []
        [ form [ classes "mb-3 row g-3" ]
            [ div [ class "col-md-3" ]
                [ label [ class "form-label", for "itemValueForm" ] [ text itemValueLabel ]
                , input
                    [ class "form-control"
                    , id "itemValueForm"
                    , type_ "number"
                    , Html.Attributes.min "1"
                    , Html.Attributes.max "30000000"
                    , placeholder itemValueLabel
                    , attribute "aria-label" itemValueLabel
                    , onInput <| String.toInt >> Maybe.withDefault 0 >> ItemValueForm
                    , value <| String.fromInt model.itemValue
                    ]
                    []
                ]
            ]
        , form [ classes "mb-3 row g-3" ]
            [ div [ class "col-md-3" ]
                [ label [ class "form-label", for "additionalCostsForm" ] [ text additionalCostsLabel ]
                , input
                    [ class "form-control"
                    , id "additionalCostsForm"
                    , type_ "number"
                    , Html.Attributes.min "0"
                    , placeholder additionalCostsLabel
                    , attribute "aria-label" additionalCostsLabel
                    , onInput <| String.toInt >> Maybe.withDefault 0 >> AdditionalCostsForm
                    , value <| String.fromInt model.additionalCosts
                    ]
                    []
                ]
            ]
        , form [ classes "mb-3 row g-3" ]
            [ div [ class "col-md-3" ]
                [ label [ class "form-label", for "instancesForm" ] [ text instancesLabel ]
                , select
                    [ class "form-select"
                    , id "instancesForm"
                    , attribute "aria-label" instancesLabel
                    , onInput (strToInstances >> InstancesForm)
                    ]
                    [ option [ value "OneInstance" ] [ text "Eine Instanz" ]
                    , option [ value "TwoInstances" ] [ text "Zwei Instanzen" ]
                    ]
                ]
            ]
        , form [ classes "mb-3 row g-3" ]
            [ div [ class "col-md-3" ]
                [ div [ class "form-check" ]
                    [ input
                        [ class "form-check-input"
                        , id "vatCustomerCheckbox"
                        , type_ "checkbox"
                        , value ""
                        , checked model.vatCustomer
                        , onCheck VatCustomerForm
                        ]
                        []
                    , label [ class "form-check-label", for "vatCustomerCheckbox" ]
                        [ text vatCustomerLabel ]
                    ]
                ]
            , div [ class "col-md-3" ]
                [ div [ class "form-check" ]
                    [ input
                        [ class "form-check-input"
                        , id "vatOpponentCheckbox"
                        , type_ "checkbox"
                        , value ""
                        , checked model.vatOpponent
                        , onCheck VatOpponentForm
                        ]
                        []
                    , label [ class "form-check-label", for "vatOpponentCheckbox" ]
                        [ text vatOpponentLabel ]
                    ]
                ]
            ]
        , form [ classes "mb-3 row g-3" ]
            [ div [ class "col-md-3" ]
                [ label [ class "form-label", for "probabilityForm" ] [ text probabilityLabel ]
                , div [ class "input-group" ]
                    [ input
                        [ class "form-control"
                        , id "probabilityForm"
                        , type_ "number"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "100"
                        , placeholder probabilityLabel
                        , attribute "aria-label" probabilityLabel
                        , onInput <| String.toInt >> Maybe.withDefault 0 >> ProbabilityForm
                        , value <| String.fromInt model.probability
                        ]
                        []
                    , span [ class "input-group-text" ] [ text "%" ]
                    ]
                ]
            ]
        ]


result : Model -> Html Msg
result model =
    let
        ( firstInstanceHtml, totalFirst ) =
            resultFirstInstance model

        ( secondInstanceHtmlList, totalSecond ) =
            case model.instances of
                TwoInstances ->
                    resultSecondInstance model

                OneInstance ->
                    ( [], 0.0 )

        sumOfAllCosts : Float
        sumOfAllCosts =
            totalFirst + totalSecond

        expectation : Float
        expectation =
            (toFloat (model.itemValue * model.probability) / 100) - (sumOfAllCosts * (toFloat <| 100 - model.probability) / 100)

        expectationText =
            if expectation > 0 then
                "Die Sache lohnt sich, weil der Erwartungswert positiv ist."

            else
                "Die Sache lohnt sich nicht, weil der Erwartungswert nicht positiv ist."
    in
    div []
        [ div [ class "mb-5" ]
            [ h3 [ class "mb-3" ] [ text "Gesamtkosten" ]
            , p [] [ text <| "Die Kosten des Rechtsstreits betragen ", strong [] [ text <| "EUR " ++ stringFromFloatGermanWithDecimals sumOfAllCosts ], text "." ]
            , p [] [ text "Enthalten sind die Verfahrens- und Termingebühren der Instanz(en), die Pauschale nach Nr. 7002 VV RVG und ggf. die Umsatzsteuer für zwei Rechtsanwälte sowie die Gerichtskosten." ]
            , p [] [ text "Nicht enthalten sind Mehrkosten bei mehreren Mandanten/Gegnern, Einigungsgebühren und streitwertunabhängige Kosten wie Reisekosten und sonstige Auslagen." ]
            , p [] [ text <| "Der Erwartungswert beträgt EUR " ++ stringFromFloatGermanWithDecimals expectation ++ ". ", strong [] [ text expectationText ] ]
            ]
        , div [ class "mb-5" ]
            ([ h3 [ class "mb-3" ] [ text "Einzelaufstellung" ]
             , firstInstanceHtml
             ]
                ++ secondInstanceHtmlList
            )
        ]


resultFirstInstance : Model -> ( Html Msg, Float )
resultFirstInstance model =
    let
        ra13 : Float
        ra13 =
            gebuehrRvgCalc model.itemValue 1.3

        ra12 : Float
        ra12 =
            gebuehrRvgCalc model.itemValue 1.2

        vatCustomer : Float
        vatCustomer =
            if model.vatCustomer then
                (ra13 + ra12 + 20) |> getVat19

            else
                0

        vatOpponent : Float
        vatOpponent =
            if model.vatOpponent then
                (ra13 + ra12 + 20) |> getVat19

            else
                0

        totalRaCustomer : Float
        totalRaCustomer =
            ra13 + ra12 + 20 + vatCustomer

        totalRaOpponent : Float
        totalRaOpponent =
            ra13 + ra12 + 20 + vatOpponent

        gkg3 : Float
        gkg3 =
            gebuehrGkgCalc model.itemValue 3.0

        totalGkg : Float
        totalGkg =
            toFloat model.additionalCosts + gkg3

        superTotal : Float
        superTotal =
            totalRaCustomer + totalRaOpponent + totalGkg
    in
    ( div [ classes "mb-3 row g-3" ]
        [ div [ class "col-md-5" ]
            [ h4 [] [ text "Rechtsanwalt Mandant (I)" ]
            , table [ class "table" ]
                [ tr [] [ td [] [ span [] [ text "1,3 Verfahrensgebühr Nr. 3100 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals ra13 ] ] ]
                , tr [] [ td [] [ span [] [ text "1,2 Terminsgebühr Nr. 3104 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals ra12 ] ] ]
                , tr [] [ td [] [ span [] [ text "Pauschale Nr. 7002 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text "20,00" ] ] ]
                , tr [] [ td [] [ span [] [ text "Umsatzsteuer 19 % Nr. 7008 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals vatCustomer ] ] ]
                , tr [] [ td [] [ strong [] [ text "Summe:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ strong [] [ span [] [ text <| stringFromFloatGermanWithDecimals totalRaCustomer ] ] ] ]
                ]
            ]
        , div [ class "col-md-5" ]
            [ h4 [] [ text "Rechtsanwalt Gegenseite (I)" ]
            , table [ class "table" ]
                [ tr [] [ td [] [ span [] [ text "1,3 Verfahrensgebühr Nr. 3100 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals ra13 ] ] ]
                , tr [] [ td [] [ span [] [ text "1,2 Terminsgebühr Nr. 3104 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals ra12 ] ] ]
                , tr [] [ td [] [ span [] [ text "Pauschale Nr. 7002 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text "20,00" ] ] ]
                , tr [] [ td [] [ span [] [ text "Umsatzsteuer 19 % Nr. 7008 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals vatOpponent ] ] ]
                , tr [] [ td [] [ strong [] [ text "Summe:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ strong [] [ span [] [ text <| stringFromFloatGermanWithDecimals totalRaOpponent ] ] ] ]
                ]
            ]
        , div [ class "col-md-5" ]
            [ h4 [] [ text "Gericht (I)" ]
            , table [ class "table" ]
                [ tr [] [ td [] [ span [] [ text "3,0 Verfahrensgebühr Nr. 1210 KV GKG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals gkg3 ] ] ]
                , tr [] [ td [] [ span [] [ text "Sonstige Gerichtskosten, z. B. für Gutachten:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals (toFloat model.additionalCosts) ] ] ]
                , tr [] [ td [] [ strong [] [ text "Summe:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ strong [] [ span [ class "text-end" ] [ text <| stringFromFloatGermanWithDecimals totalGkg ] ] ] ]
                ]
            ]
        ]
    , superTotal
    )


resultSecondInstance : Model -> ( List (Html Msg), Float )
resultSecondInstance model =
    let
        ra16 : Float
        ra16 =
            gebuehrRvgCalc model.itemValue 1.6

        ra12 : Float
        ra12 =
            gebuehrRvgCalc model.itemValue 1.2

        vatCustomer : Float
        vatCustomer =
            if model.vatCustomer then
                (ra16 + ra12 + 20) |> getVat19

            else
                0

        vatOpponent : Float
        vatOpponent =
            if model.vatOpponent then
                (ra16 + ra12 + 20) |> getVat19

            else
                0

        totalRaCustomer : Float
        totalRaCustomer =
            ra16 + ra12 + 20 + vatCustomer

        totalRaOpponent : Float
        totalRaOpponent =
            ra16 + ra12 + 20 + vatOpponent

        gkg4 : Float
        gkg4 =
            gebuehrGkgCalc model.itemValue 4.0

        totalGkg : Float
        totalGkg =
            toFloat model.additionalCosts + gkg4

        superTotal : Float
        superTotal =
            totalRaCustomer + totalRaOpponent + totalGkg
    in
    ( [ div [ classes "mb-3 row g-3" ]
            [ div [ class "col-md-5" ]
                [ h4 [] [ text "Rechtsanwalt Mandant (II)" ]
                , table [ class "table" ]
                    [ tr [] [ td [] [ span [] [ text "1,6 Verfahrensgebühr Nr. 3200 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals ra16 ] ] ]
                    , tr [] [ td [] [ span [] [ text "1,2 Terminsgebühr Nr. 3202 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals ra12 ] ] ]
                    , tr [] [ td [] [ span [] [ text "Pauschale Nr. 7002 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text "20,00" ] ] ]
                    , tr [] [ td [] [ span [] [ text "Umsatzsteuer 19 % Nr. 7008 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals vatCustomer ] ] ]
                    , tr [] [ td [] [ strong [] [ text "Summe:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ strong [] [ span [] [ text <| stringFromFloatGermanWithDecimals totalRaCustomer ] ] ] ]
                    ]
                ]
            , div [ class "col-md-5" ]
                [ h4 [] [ text "Rechtsanwalt Gegenseite (II)" ]
                , table [ class "table" ]
                    [ tr [] [ td [] [ span [] [ text "1,6 Verfahrensgebühr Nr. 3200 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals ra16 ] ] ]
                    , tr [] [ td [] [ span [] [ text "1,2 Terminsgebühr Nr. 3202 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals ra12 ] ] ]
                    , tr [] [ td [] [ span [] [ text "Pauschale Nr. 7002 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text "20,00" ] ] ]
                    , tr [] [ td [] [ span [] [ text "Umsatzsteuer 19 % Nr. 7008 VV RVG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals vatOpponent ] ] ]
                    , tr [] [ td [] [ strong [] [ text "Summe:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ strong [] [ span [] [ text <| stringFromFloatGermanWithDecimals totalRaOpponent ] ] ] ]
                    ]
                ]
            , div [ class "col-md-5" ]
                [ h4 [] [ text "Gericht (II)" ]
                , table [ class "table" ]
                    [ tr [] [ td [] [ span [] [ text "4,0 Verfahrensgebühr Nr. 1220 KV GKG:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals gkg4 ] ] ]
                    , tr [] [ td [] [ span [] [ text "Sonstige Gerichtskosten, z. B. für Gutachten:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ span [] [ text <| stringFromFloatGermanWithDecimals (toFloat model.additionalCosts) ] ] ]
                    , tr [] [ td [] [ strong [] [ text "Summe:" ] ], td [] [ span [] [ text "EUR" ] ], td [ class "text-end" ] [ strong [] [ span [ class "text-end" ] [ text <| stringFromFloatGermanWithDecimals totalGkg ] ] ] ]
                    ]
                ]
            ]
      ]
    , superTotal
    )


gebuehrRvg : Int -> Int
gebuehrRvg itemValue =
    let
        fn : Int -> Int -> Int -> Int
        fn k v b =
            if itemValue <= k then
                v

            else
                b
    in
    if itemValue > 500000 then
        let
            i : Float
            i =
                toFloat (itemValue - 500000) / 50000
        in
        3539 + (ceiling i * 165)

    else
        gebuehrentabelleRvg |> Dict.foldr fn 0


gebuehrentabelleRvg : Dict.Dict Int Int
gebuehrentabelleRvg =
    Dict.fromList
        [ ( 500, 49 )
        , ( 1000, 88 )
        , ( 1500, 127 )
        , ( 2000, 166 )
        , ( 3000, 222 )
        , ( 4000, 278 )
        , ( 5000, 334 )
        , ( 6000, 390 )
        , ( 7000, 446 )
        , ( 8000, 502 )
        , ( 9000, 558 )
        , ( 10000, 614 )
        , ( 13000, 666 )
        , ( 16000, 718 )
        , ( 19000, 770 )
        , ( 22000, 822 )
        , ( 25000, 874 )
        , ( 30000, 955 )
        , ( 35000, 1036 )
        , ( 40000, 1117 )
        , ( 45000, 1198 )
        , ( 50000, 1279 )
        , ( 65000, 1373 )
        , ( 80000, 1467 )
        , ( 95000, 1561 )
        , ( 110000, 1655 )
        , ( 125000, 1749 )
        , ( 140000, 1843 )
        , ( 155000, 1937 )
        , ( 170000, 2031 )
        , ( 185000, 2125 )
        , ( 200000, 2219 )
        , ( 230000, 2351 )
        , ( 260000, 2483 )
        , ( 290000, 2615 )
        , ( 320000, 2747 )
        , ( 350000, 2879 )
        , ( 380000, 3011 )
        , ( 410000, 3143 )
        , ( 440000, 3275 )
        , ( 470000, 3407 )
        , ( 500000, 3539 )
        ]


gebuehrRvgCalc : Int -> Float -> Float
gebuehrRvgCalc itemValue factor =
    toFloat (gebuehrRvg itemValue) * factor


getVat19 : Float -> Float
getVat19 v =
    v * 0.19


gebuehrGkg : Int -> Int
gebuehrGkg itemValue =
    let
        fn : Int -> Int -> Int -> Int
        fn k v b =
            if itemValue <= k then
                v

            else
                b
    in
    if itemValue > 500000 then
        let
            i : Float
            i =
                toFloat (itemValue - 500000) / 50000
        in
        3901 + (ceiling i * 198)

    else
        gebuehrentabelleGkg |> Dict.foldr fn 0


gebuehrentabelleGkg : Dict.Dict Int Int
gebuehrentabelleGkg =
    Dict.fromList
        [ ( 500, 38 )
        , ( 1000, 58 )
        , ( 1500, 78 )
        , ( 2000, 98 )
        , ( 3000, 119 )
        , ( 4000, 140 )
        , ( 5000, 161 )
        , ( 6000, 182 )
        , ( 7000, 203 )
        , ( 8000, 224 )
        , ( 9000, 245 )
        , ( 10000, 266 )
        , ( 13000, 295 )
        , ( 16000, 324 )
        , ( 19000, 353 )
        , ( 22000, 382 )
        , ( 25000, 411 )
        , ( 30000, 449 )
        , ( 35000, 487 )
        , ( 40000, 525 )
        , ( 45000, 563 )
        , ( 50000, 601 )
        , ( 65000, 733 )
        , ( 80000, 865 )
        , ( 95000, 997 )
        , ( 110000, 1129 )
        , ( 125000, 1261 )
        , ( 140000, 1393 )
        , ( 155000, 1525 )
        , ( 170000, 1657 )
        , ( 185000, 1789 )
        , ( 200000, 1921 )
        , ( 230000, 2119 )
        , ( 260000, 2317 )
        , ( 290000, 2515 )
        , ( 320000, 2713 )
        , ( 350000, 2911 )
        , ( 380000, 3109 )
        , ( 410000, 3307 )
        , ( 440000, 3505 )
        , ( 470000, 3703 )
        , ( 500000, 3901 )
        ]


gebuehrGkgCalc : Int -> Float -> Float
gebuehrGkgCalc itemValue factor =
    toFloat (gebuehrGkg itemValue) * factor


stringFromFloatGermanWithDecimals : Float -> String
stringFromFloatGermanWithDecimals f =
    let
        s : String
        s =
            (f * 100 |> round |> toFloat) / 100 |> String.fromFloat |> String.replace "." ","

        l : Int
        l =
            String.length s
    in
    case s |> String.indexes "," of
        [] ->
            s ++ ",00"

        [ x ] ->
            if x == l - 2 then
                s ++ "0"

            else
                s

        _ ->
            s



-- HELPERS


{-| This helper takes a string with class names separated by one whitespace. All
classes are applied to the result.

    import Html exposing (..)

    view : Model -> Html msg
    view model =
        div [ classes "center with-border nice-color" ] [ text model.content ]

-}
classes : String -> Html.Attribute msg
classes s =
    let
        cl : List ( String, Bool )
        cl =
            String.split " " s |> List.map (\c -> ( c, True ))
    in
    classList cl
