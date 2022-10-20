module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, classList, default, placeholder, rows, selected, type_, value)
import Html.Events exposing (onCheck, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { client : Client
    , opponentGreeting : OpponentGreeting
    , legalReason : LegalReason
    , timeOfDelay : TimeOfDelay
    , defaultInterest : DefaultInterest
    , rightToDeductInputTax : RightToDeductInputTax
    }


init : Model
init =
    Model
        (initClient SwitchClientFormNaturalPerson)
        GreetingCommon
        "aus dem mit Ihnen geschlossenen Liefervertrag/Werkvertrag/...vertrag vom ... gemäß Rechnung Nr. ... vom ..."
        "TT.MM.JJJJ"
        LegalDefaultInterest
        False


initClient : SwitchClientForm -> Client
initClient scf =
    case scf of
        SwitchClientFormNaturalPerson ->
            NaturalPerson Male "Max Mustermann" "Musterstraße 1, 12345 Musterstadt"

        SwitchClientFormLegalEntity ->
            LegalEntity Die "Muster GmbH" "Musterstraße 1, 12345 Musterstadt"


type Client
    = NaturalPerson Gender Name Address
    | LegalEntity Grammar Name Address


type Gender
    = Male
    | Female
    | Undefined


type Grammar
    = Der
    | Die


type alias Name =
    String


type alias Address =
    String


type OpponentGreeting
    = GreetingSir String
    | GreetingMadame String
    | GreetingCommon


type alias LegalReason =
    String


type alias TimeOfDelay =
    String


type DefaultInterest
    = LegalDefaultInterest
    | HigherDefaultInterest String


type alias RightToDeductInputTax =
    Bool



-- UPDATE


type Msg
    = ClientForm ClientForm
    | OpponentGreetingForm OpponentGreetingForm
    | LegalReasonForm LegalReason
    | TimeOfDelayForm TimeOfDelay
    | DefaultInterestForm DefaultInterestForm
    | RightToDeductInputTaxForm RightToDeductInputTax


type ClientForm
    = SwitchClientForm SwitchClientForm
    | NaturalPersonForm NaturalPersonForm
    | LegalEntityForm LegalEntityForm


type SwitchClientForm
    = SwitchClientFormNaturalPerson
    | SwitchClientFormLegalEntity


type NaturalPersonForm
    = NaturalPersonFormGender Gender
    | NaturalPersonFormName Name
    | NaturalPersonFormAddress Address


type LegalEntityForm
    = LegalEntityFormGrammar Grammar
    | LegalEntityFormName String
    | LegalEntityFormAddress String


type OpponentGreetingForm
    = OpponentGreetingFormGreetingSir String
    | OpponentGreetingFormGreetingMadame String
    | OpponentGreetingFormGreetingCommon


type DefaultInterestForm
    = DefaultInterestFormLegalDefaultInterest
    | DefaultInterestFormHigherDefaultInterest String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClientForm cf ->
            case cf of
                NaturalPersonForm npf ->
                    case model.client of
                        NaturalPerson g n a ->
                            case npf of
                                NaturalPersonFormGender newGender ->
                                    { model | client = NaturalPerson newGender n a }

                                NaturalPersonFormName newName ->
                                    { model | client = NaturalPerson g newName a }

                                NaturalPersonFormAddress newAddress ->
                                    { model | client = NaturalPerson g n newAddress }

                        LegalEntity _ _ _ ->
                            model

                LegalEntityForm lef ->
                    case model.client of
                        NaturalPerson _ _ _ ->
                            model

                        LegalEntity g n a ->
                            case lef of
                                LegalEntityFormGrammar newGrammar ->
                                    { model | client = LegalEntity newGrammar n a }

                                LegalEntityFormName newName ->
                                    { model | client = LegalEntity g newName a }

                                LegalEntityFormAddress newAddress ->
                                    { model | client = LegalEntity g n newAddress }

                SwitchClientForm scf ->
                    { model | client = initClient scf }

        OpponentGreetingForm ogf ->
            case ogf of
                OpponentGreetingFormGreetingSir txt ->
                    { model | opponentGreeting = GreetingSir txt }

                OpponentGreetingFormGreetingMadame txt ->
                    { model | opponentGreeting = GreetingMadame txt }

                OpponentGreetingFormGreetingCommon ->
                    { model | opponentGreeting = GreetingCommon }

        TimeOfDelayForm todf ->
            { model | timeOfDelay = todf }

        LegalReasonForm lrf ->
            { model | legalReason = lrf }

        DefaultInterestForm dif ->
            case dif of
                DefaultInterestFormLegalDefaultInterest ->
                    { model | defaultInterest = LegalDefaultInterest }

                DefaultInterestFormHigherDefaultInterest s ->
                    { model | defaultInterest = HigherDefaultInterest s }

        RightToDeductInputTaxForm rdit ->
            { model | rightToDeductInputTax = rdit }



-- VIEW


view : Model -> Html Msg
view model =
    div [ classes "container p-3 pb-5" ]
        [ main_ []
            [ h1 [] [ text "TextBauStein Mahnschreiben" ]
            , div [ class "mb-5" ]
                [ h2 [ class "mb-3" ] [ text "Eingaben" ]
                , modelInput model
                ]
            , div []
                [ h2 [ class "mb-3" ] [ text "Ergebnis" ]
                , result model
                ]
            ]
        ]



-- FORMS


modelInput : Model -> Html Msg
modelInput model =
    div []
        [ clientForm model.client |> map ClientForm
        , opponenGreetingForm model.opponentGreeting |> map OpponentGreetingForm
        , legalReasonForm model.legalReason
        , timeOfDelayForm model.timeOfDelay
        , defaultInterestForm model.defaultInterest |> map DefaultInterestForm
        , rightToDeductInputTaxForm model.rightToDeductInputTax
        ]


clientForm : Client -> Html ClientForm
clientForm client =
    let
        innerForm : Html ClientForm
        innerForm =
            case client of
                NaturalPerson g n a ->
                    form [ class "mb-3" ]
                        [ div [ classes "row g-3" ]
                            [ div [ class "col-md-3" ]
                                [ label [ class "form-label" ] [ text "Anrede" ]
                                , select
                                    [ class "form-select"
                                    , attribute "aria-label" "Anrede"
                                    , onInput (strToGender >> NaturalPersonFormGender)
                                    ]
                                    [ option [ value "Male", selected <| g == Male ] [ text "Herr" ]
                                    , option [ value "Female", selected <| g == Female ] [ text "Frau" ]
                                    , option [ value "Undefined", selected <| g == Undefined ] [ text "(ohne)" ]
                                    ]
                                ]
                            , div [ class "col-md-3" ]
                                [ label [ class "form-label" ] [ text "Name" ]
                                , input
                                    [ class "form-control"
                                    , type_ "text"
                                    , placeholder "Name"
                                    , attribute "aria-label" "Name"
                                    , onInput NaturalPersonFormName
                                    , value n
                                    ]
                                    []
                                ]
                            , div [ class "col-md-3" ]
                                [ label [ class "form-label" ] [ text "Adresse" ]
                                , input
                                    [ class "form-control"
                                    , type_ "text"
                                    , placeholder "Adresse"
                                    , attribute "aria-label" "Adresse"
                                    , onInput NaturalPersonFormAddress
                                    , value a
                                    ]
                                    []
                                ]
                            ]
                        ]
                        |> map NaturalPersonForm

                LegalEntity g n a ->
                    form [ class "mb-3" ]
                        [ div [ classes "row g-3" ]
                            [ div [ class "col-md-3" ]
                                [ label [ class "form-label" ] [ text "Grammatisches Geschlecht" ]
                                , select
                                    [ class "form-select"
                                    , attribute "aria-label" "Grammatisches Geschlecht"
                                    , onInput (strToGrammar >> LegalEntityFormGrammar)
                                    ]
                                    [ option [ value "Der", selected <| g == Der ] [ text "der" ]
                                    , option [ value "Die", selected <| g == Die ] [ text "die" ]
                                    ]
                                ]
                            , div [ class "col-md-3" ]
                                [ label [ class "form-label" ] [ text "Name" ]
                                , input
                                    [ class "form-control"
                                    , type_ "text"
                                    , placeholder "Name"
                                    , attribute "aria-label" "Name"
                                    , onInput LegalEntityFormName
                                    , value n
                                    ]
                                    []
                                ]
                            , div [ class "col-md-3" ]
                                [ label [ class "form-label" ] [ text "Adresse" ]
                                , input
                                    [ class "form-control"
                                    , type_ "text"
                                    , placeholder "Adresse"
                                    , attribute "aria-label" "Adresse"
                                    , onInput LegalEntityFormAddress
                                    , value a
                                    ]
                                    []
                                ]
                            ]
                        ]
                        |> map LegalEntityForm
    in
    div []
        [ form [ class "mb-3" ]
            [ div [ classes "row g-3" ]
                [ div [ class "col-md-3" ]
                    [ label [ class "form-label" ] [ text "Rechtsform unserer Mandantschaft" ]
                    , select
                        [ class "form-select"
                        , attribute "aria-label" "Rechtsform unserer Mandantschaft"
                        , onInput (strToSwitchClientForm >> SwitchClientForm)
                        ]
                        [ option [ value "NaturalPerson" ] [ text "Natürliche Person" ]
                        , option [ value "LegalEntity" ] [ text "Juristische Person / Gesellschaft" ]
                        ]
                    ]
                ]
            ]
        , innerForm
        ]


strToGender : String -> Gender
strToGender s =
    if s == "Male" then
        Male

    else if s == "Female" then
        Female

    else
        Undefined


strToGrammar : String -> Grammar
strToGrammar s =
    if s == "Der" then
        Der

    else
        Die


strToSwitchClientForm : String -> SwitchClientForm
strToSwitchClientForm s =
    if s == "NaturalPerson" then
        SwitchClientFormNaturalPerson

    else
        SwitchClientFormLegalEntity


opponenGreetingForm : OpponentGreeting -> Html OpponentGreetingForm
opponenGreetingForm opponentGreeting =
    let
        opponentGreetingCase =
            \m case_ ->
                case m of
                    GreetingSir _ ->
                        case_ == GreetingSir ""

                    GreetingMadame _ ->
                        case_ == GreetingMadame ""

                    GreetingCommon ->
                        case_ == GreetingCommon
    in
    form [ class "mb-3" ]
        [ div [ classes "row g-3" ]
            [ div [ class "col-md-3" ]
                [ label [ class "form-label" ] [ text "Anrede im Brief" ]
                , select
                    [ class "form-select"
                    , attribute "aria-label" "Anrede im Brief"
                    , onInput switchOpponentGreetingForm
                    ]
                    [ option [ value "GreetingSir", selected <| opponentGreetingCase opponentGreeting (GreetingSir "") ] [ text "Sehr geehrter Herr ...," ]
                    , option [ value "GreetingMadame", selected <| opponentGreetingCase opponentGreeting (GreetingMadame "") ] [ text "Sehr geehrte Frau ...," ]
                    , option [ value "GreetingCommon", selected <| opponentGreetingCase opponentGreeting GreetingCommon ] [ text "Sehr geehrte Damen und Herren," ]
                    ]
                ]
            , case opponentGreeting of
                GreetingSir txt ->
                    div [ class "col-md-3" ]
                        [ label [ class "form-label" ] [ text "Name des Empfängers" ]
                        , input
                            [ class "form-control"
                            , type_ "text"
                            , placeholder "Name des Empfängers"
                            , attribute "aria-label" "Name des Empfängers"
                            , onInput OpponentGreetingFormGreetingSir
                            , value txt
                            ]
                            []
                        ]

                GreetingMadame txt ->
                    div [ class "col-md-3" ]
                        [ label [ class "form-label" ] [ text "Name des Empfängers" ]
                        , input
                            [ class "form-control"
                            , type_ "text"
                            , placeholder "Name des Empfängers"
                            , attribute "aria-label" "Name des Empfängers"
                            , onInput OpponentGreetingFormGreetingMadame
                            , value txt
                            ]
                            []
                        ]

                GreetingCommon ->
                    div [ class "col-md-3" ] []
            ]
        ]


switchOpponentGreetingForm : String -> OpponentGreetingForm
switchOpponentGreetingForm s =
    if s == "GreetingSir" then
        OpponentGreetingFormGreetingSir ""

    else if s == "GreetingMadame" then
        OpponentGreetingFormGreetingMadame ""

    else
        OpponentGreetingFormGreetingCommon


legalReasonForm : LegalReason -> Html Msg
legalReasonForm legalReason =
    form [ class "mb-3" ]
        [ div [ classes "row g-3" ]
            [ div [ class "col-md-6" ]
                [ label [ class "form-label" ] [ text "Rechtsgrund der Forderung" ]
                , textarea
                    [ class "form-control"
                    , rows 2
                    , placeholder "Rechtsgrund der Forderung"
                    , attribute "aria-label" "Rechtsgrund der Forderung"
                    , onInput LegalReasonForm
                    , value legalReason
                    ]
                    []
                ]
            ]
        ]


timeOfDelayForm : TimeOfDelay -> Html Msg
timeOfDelayForm timeOfDelay =
    form [ class "mb-3" ]
        [ div [ classes "row g-3" ]
            [ div [ class "col-md-6" ]
                [ label [ class "form-label" ] [ text "Beginn des Verzugs" ]
                , input
                    [ class "form-control"
                    , type_ "text"
                    , placeholder "Beginn des Verzugs"
                    , attribute "aria-label" "Beginn des Verzugs"
                    , onInput TimeOfDelayForm
                    , value timeOfDelay
                    ]
                    []
                ]
            ]
        ]


defaultInterestForm : DefaultInterest -> Html DefaultInterestForm
defaultInterestForm defaultInterest =
    form [ class "mb-3" ]
        [ div [ classes "row g-3" ]
            [ div [ class "col-md-3" ]
                [ label [ class "form-label" ] [ text "Verzugszinsen" ]
                , select
                    [ class "form-select"
                    , attribute "aria-label" "Verzugszinsen"
                    , onInput switchDefaultInterestForm
                    ]
                    [ option [ value "LegalDefaultInterest", selected <| defaultInterest == LegalDefaultInterest ] [ text "Gesetzlicher Verzugszins" ]
                    , option [ value "HigherDefaultInterest", selected <| defaultInterest /= LegalDefaultInterest ] [ text "Höherer Verzugszins" ]
                    ]
                ]
            , case defaultInterest of
                LegalDefaultInterest ->
                    div [] []

                HigherDefaultInterest txt ->
                    div [ class "col-md-3" ]
                        [ label [ class "form-label" ] [ text "Begründung für den höheren Verzugszins" ]
                        , input
                            [ class "form-control"
                            , type_ "text"
                            , placeholder "Begründung für den höheren Verzugszins"
                            , attribute "aria-label" "Begründung für den höheren Verzugszins"
                            , onInput DefaultInterestFormHigherDefaultInterest
                            , value txt
                            ]
                            []
                        ]
            ]
        ]


switchDefaultInterestForm : String -> DefaultInterestForm
switchDefaultInterestForm s =
    if s == "LegalDefaultInterest" then
        DefaultInterestFormLegalDefaultInterest

    else
        DefaultInterestFormHigherDefaultInterest "..."


rightToDeductInputTaxForm : RightToDeductInputTax -> Html Msg
rightToDeductInputTaxForm rightToDeductInputTax =
    form [ class "mb-3" ]
        [ div [ class "form-check" ]
            [ input [ class "form-check-input", type_ "checkbox", value "", checked rightToDeductInputTax, onCheck RightToDeductInputTaxForm ]
                []
            , label [ class "form-check-label" ]
                [ text "Vorsteuerabzugsberechtigung unserer Mandantschaft" ]
            ]
        ]



-- RESULT


result : Model -> Html Msg
result model =
    div [ class "col-8" ]
        [ p [ class "pt-3" ] (rubrum model.client)
        , p [] [ text <| greeting model.opponentGreeting ]
        , p [] [ text <| representation model.client ]
        , p [] [ text <| claim model.client model.legalReason ]
        , p [] [ text <| default model.client model.timeOfDelay ]
        , p [] [ text <| defaultInterestText model.defaultInterest ]
        , p [] [ text <| requestForPayment model.client ]
        , p [] [ text "Da Sie sich im Verzug befinden, schulden Sie auch ..." ]
        , p [] [ text "RVG Tabelle" ]
        , p [] [ text <| rightToDeductInputTaxText model.client model.rightToDeductInputTax ]
        , p [] [ text "Sie können die Freistellung durch Erklärung oder dadurch bewirken, dass Sie den Betrag unter Angabe des Aktenzeichens auf unser auf Seite es dieses Schreibens unten angegebenes Geschäftskonto überweisen." ]
        , p [] [ text "Weitere Inkassokosten werden derzeit nicht geltend gemacht." ]
        , p [] [ text "Die für uns zuständige Rechtsanwaltskammer ist die Rechtsanwaltskammer Sachsen, Glacisstraße 6, 01099 Dresden. Die E-Mail-Adresse der Rechtsanwaltskammer Sachsen lautet info@rak-sachsen.de." ]
        , p [] [ text "Mit freundlichen Grüßen" ]
        ]


rubrum : Client -> List (Html Msg)
rubrum client =
    let
        name : Name
        name =
            case client of
                NaturalPerson _ n _ ->
                    n

                LegalEntity _ n _ ->
                    n
    in
    [ strong [] [ text <| name ++ " ./. ..." ], br [] [], strong [] [ text "wegen ..." ] ]


greeting : OpponentGreeting -> String
greeting opponentGreeting =
    case opponentGreeting of
        GreetingSir n ->
            "Sehr geehrter Herr " ++ n ++ ","

        GreetingMadame n ->
            "Sehr geehrte Frau " ++ n ++ ","

        GreetingCommon ->
            "Sehr geehrte Damen und Herren,"


representation : Client -> String
representation client =
    let
        mdt =
            case client of
                NaturalPerson g n a ->
                    (case g of
                        Male ->
                            "mein Mandant, Herr "

                        Female ->
                            "meine Mandantin, Frau "

                        Undefined ->
                            "meine Mandantschaft, "
                    )
                        ++ n
                        ++ ", "
                        ++ a
                        ++ ","

                LegalEntity g n a ->
                    (case g of
                        Der ->
                            "mein Mandant, der "

                        Die ->
                            "meine Mandantin, die "
                    )
                        ++ n
                        ++ ", "
                        ++ a
                        ++ ","
    in
    "in der oben genannten Angelegenheit hat uns "
        ++ mdt
        ++ " beauftragt und bevollmächtigt."


claim : Client -> String -> String
claim client legalReason =
    "Sie schulden " ++ clientDative client ++ " " ++ legalReason ++ " noch einen Betrag in Höhe von EUR ..."


default : Client -> TimeOfDelay -> String
default client timeOfDelay =
    "Vertraglich war vereinbart, dass Sie die Forderung "
        ++ clientGenitive client
        ++ " binnen ... Tagen nach Rechnungslegung begleichen. Sie sind deshalb seit dem "
        ++ timeOfDelay
        ++ " im Verzug."


defaultInterestText : DefaultInterest -> String
defaultInterestText defaultInterestValue =
    "Weil Sie im Verzug sind, schulden Sie zusätzlich Verzugszinsen"
        ++ (case defaultInterestValue of
                LegalDefaultInterest ->
                    " in gesetzlicher Höhe."

                HigherDefaultInterest t ->
                    ". Der Zinssatz liegt über dem gesetzlichen Verzugszins, weil " ++ t ++ "."
           )
        ++ " Die Zinsberechnung entnehmen Sie bitte der beiliegenden Forderungsaufstellung."


requestForPayment : Client -> String
requestForPayment client =
    "Namens "
        ++ clientGenitive client
        ++ " fordere ich Sie auf, den aus der Forderungsaufstellung ersichtlichen Gesamtbetrag "
        ++ "in Höhe von EUR ... binnen 10 Tagen auf das Konto "
        ++ clientGenitive client
        ++ " mit der IBAN ... zu überweisen."


rightToDeductInputTaxText : Client -> RightToDeductInputTax -> String
rightToDeductInputTaxText client rightToDeductInputTax =
    if not rightToDeductInputTax then
        (clientNominative client
            |> String.left 1
            |> String.toUpper
        )
            ++ (clientNominative client |> String.dropLeft 1)
            ++ " ist zum Vorsteuerabzug nicht berechtigt."

    else
        ""



-- HELPERS


clientNominative : Client -> String
clientNominative client =
    case client of
        NaturalPerson g _ _ ->
            case g of
                Male ->
                    "mein Mandant"

                Female ->
                    "meine Mandantin"

                Undefined ->
                    "meine Mandantschaft"

        LegalEntity g _ _ ->
            case g of
                Der ->
                    "mein Mandant"

                Die ->
                    "meine Mandantin"


clientGenitive : Client -> String
clientGenitive client =
    case client of
        NaturalPerson g _ _ ->
            case g of
                Male ->
                    "meines Mandanten"

                Female ->
                    "meiner Mandantin"

                Undefined ->
                    "meiner Mandantschaft"

        LegalEntity g _ _ ->
            case g of
                Der ->
                    "meines Mandanten"

                Die ->
                    "meiner Mandantin"


clientDative : Client -> String
clientDative client =
    case client of
        NaturalPerson g _ _ ->
            case g of
                Male ->
                    "meinem Mandanten"

                Female ->
                    "meiner Mandantin"

                Undefined ->
                    "meiner Mandantschaft"

        LegalEntity g _ _ ->
            case g of
                Der ->
                    "meinem Mandanten"

                Die ->
                    "meiner Mandantin"


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
