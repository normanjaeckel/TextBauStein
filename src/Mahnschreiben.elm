module Mahnschreiben exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, classList, default, for, id, placeholder, rows, selected, type_, value)
import Html.Events exposing (onCheck, onInput)



-- MODEL


type alias Model =
    { referenceNumber : ReferenceNumber
    , client : Client
    , opponentGreeting : OpponentGreeting
    , legalReason : LegalReason
    , principalAmount : PrincipalAmount
    , sumOfAmount : SumOfAmount
    , paymentToRepresentative : PaymentToRepresentative
    , agreementOfBeginningOfDelay : AgreementOfBeginningOfDelay
    , timeOfDelay : TimeOfDelay
    , defaultInterest : DefaultInterest
    , rightToDeductInputTax : RightToDeductInputTax
    }



-- TODO: Zahlung an uns oder an Mandant mit IBAN-Feld;


init : Model
init =
    Model
        ""
        (initClient SwitchClientFormNaturalPerson)
        GreetingCommon
        "aus dem mit Ihnen geschlossenen Liefervertrag/Werkvertrag/...vertrag vom ... gemäß Rechnung Nr. ... vom ..."
        "0,00"
        "0,00"
        False
        "10"
        "..."
        LegalDefaultInterest
        False


type alias ReferenceNumber =
    String


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


type alias PrincipalAmount =
    String


type alias SumOfAmount =
    String


type alias PaymentToRepresentative =
    Bool


type alias AgreementOfBeginningOfDelay =
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
    = ReferenceNumberForm ReferenceNumber
    | ClientForm ClientForm
    | OpponentGreetingForm OpponentGreetingForm
    | LegalReasonForm LegalReason
    | PrincipalAmountForm PrincipalAmount
    | SumOfAmountForm SumOfAmount
    | PaymentToRepresentativeForm PaymentToRepresentative
    | AgreementOfBeginningOfDelayForm AgreementOfBeginningOfDelay
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
        ReferenceNumberForm rnf ->
            { model | referenceNumber = rnf }

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

        AgreementOfBeginningOfDelayForm aobdf ->
            { model | agreementOfBeginningOfDelay = aobdf }

        TimeOfDelayForm tod ->
            { model | timeOfDelay = tod }

        LegalReasonForm lr ->
            { model | legalReason = lr }

        PrincipalAmountForm pa ->
            { model | principalAmount = pa }

        SumOfAmountForm sa ->
            { model | sumOfAmount = sa }

        PaymentToRepresentativeForm ptr ->
            { model | paymentToRepresentative = ptr }

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
            [ h1 [ class "mb-3" ] [ text "TextBauStein Mahnschreiben" ]
            , div [ class "mb-5" ]
                [ h2 [ class "mb-3" ] [ text "Eingaben" ]
                , modelInput model
                ]
            , div []
                [ h2 [ class "mb-3" ] [ text "Ergebnis" ]
                , p []
                    [ em []
                        [ text <|
                            "Nachdem man oben alles eingegeben hat, kann man den Text markieren, kopieren und in den Briefkopf einfügen. "
                                ++ "Danach sind noch Adresse, Aktenzeichen und die Betreffzeile sowie die Zahlungsdaten und die RVG-Berechnung anzupassen."
                        ]
                    ]
                , result model
                ]
            ]
        ]



-- FORMS


modelInput : Model -> Html Msg
modelInput model =
    div []
        [ referenceNumberForm model.referenceNumber
        , clientForm model.client |> map ClientForm
        , rightToDeductInputTaxForm model.rightToDeductInputTax
        , opponenGreetingForm model.opponentGreeting |> map OpponentGreetingForm
        , legalReasonForm model.legalReason
        , amountForm model.principalAmount model.sumOfAmount model.paymentToRepresentative
        , delayForm model.agreementOfBeginningOfDelay model.timeOfDelay model.defaultInterest
        ]


referenceNumberForm : ReferenceNumber -> Html Msg
referenceNumberForm referenceNumber =
    let
        labelText : String
        labelText =
            "Unser Aktenzeichen"
    in
    form [ classes "mb-3 row g-3" ]
        [ div [ class "col-md-3" ]
            [ label [ class "form-label", for "referenceNumberForm" ] [ text labelText ]
            , input
                [ class "form-control"
                , id "referenceNumberForm"
                , type_ "text"
                , placeholder labelText
                , attribute "aria-label" labelText
                , onInput ReferenceNumberForm
                , value referenceNumber
                ]
                []
            ]
        ]


clientForm : Client -> Html ClientForm
clientForm client =
    let
        innerForm : Html ClientForm
        innerForm =
            case client of
                NaturalPerson g n a ->
                    form [ class "mb-3 row g-3" ]
                        [ div [ class "col-md-3" ]
                            [ label [ class "form-label", for "clientFormNaturalPersonGender" ] [ text "Anrede" ]
                            , select
                                [ class "form-select"
                                , id "clientFormNaturalPersonGender"
                                , attribute "aria-label" "Anrede"
                                , onInput (strToGender >> NaturalPersonFormGender)
                                ]
                                [ option [ value "Male", selected <| g == Male ] [ text "Herr" ]
                                , option [ value "Female", selected <| g == Female ] [ text "Frau" ]
                                , option [ value "Undefined", selected <| g == Undefined ] [ text "(ohne)" ]
                                ]
                            ]
                        , div [ class "col-md-3" ]
                            [ label [ class "form-label", for "clientFormNaturalPersonName" ] [ text "Name" ]
                            , input
                                [ class "form-control"
                                , id "clientFormNaturalPersonName"
                                , type_ "text"
                                , placeholder "Name"
                                , attribute "aria-label" "Name"
                                , onInput NaturalPersonFormName
                                , value n
                                ]
                                []
                            ]
                        , div [ class "col-md-3" ]
                            [ label [ class "form-label", for "clientFormNaturalPersonAddress" ] [ text "Adresse" ]
                            , input
                                [ class "form-control"
                                , id "clientFormNaturalPersonAddress"
                                , type_ "text"
                                , placeholder "Adresse"
                                , attribute "aria-label" "Adresse"
                                , onInput NaturalPersonFormAddress
                                , value a
                                ]
                                []
                            ]
                        ]
                        |> map NaturalPersonForm

                LegalEntity g n a ->
                    form [ classes "mb-3 row g-3" ]
                        [ div [ class "col-md-3" ]
                            [ label [ class "form-label", for "clientFormLegalEntityGrammar" ] [ text "Grammatisches Geschlecht" ]
                            , select
                                [ class "form-select"
                                , id "clientFormLegalEntityGrammar"
                                , attribute "aria-label" "Grammatisches Geschlecht"
                                , onInput (strToGrammar >> LegalEntityFormGrammar)
                                ]
                                [ option [ value "Der", selected <| g == Der ] [ text "der" ]
                                , option [ value "Die", selected <| g == Die ] [ text "die" ]
                                ]
                            ]
                        , div [ class "col-md-3" ]
                            [ label [ class "form-label", for "clientFormLegalEntityName" ] [ text "Name" ]
                            , input
                                [ class "form-control"
                                , id "clientFormLegalEntityName"
                                , type_ "text"
                                , placeholder "Name"
                                , attribute "aria-label" "Name"
                                , onInput LegalEntityFormName
                                , value n
                                ]
                                []
                            ]
                        , div [ class "col-md-3" ]
                            [ label [ class "form-label", for "clientFormLegalEntityAddress" ] [ text "Adresse" ]
                            , input
                                [ class "form-control"
                                , id "clientFormLegalEntityAddress"
                                , type_ "text"
                                , placeholder "Adresse"
                                , attribute "aria-label" "Adresse"
                                , onInput LegalEntityFormAddress
                                , value a
                                ]
                                []
                            ]
                        ]
                        |> map LegalEntityForm

        labelTextGeneral : String
        labelTextGeneral =
            "Rechtsform unserer Mandantschaft"
    in
    div []
        [ form [ classes "mb-3 row g-3" ]
            [ div [ class "col-md-3" ]
                [ label [ class "form-label", for "clientFormSwitchClientForm" ] [ text labelTextGeneral ]
                , select
                    [ class "form-select"
                    , id "clientFormSwitchClientForm"
                    , attribute "aria-label" labelTextGeneral
                    , onInput (strToSwitchClientForm >> SwitchClientForm)
                    ]
                    [ option [ value "NaturalPerson" ] [ text "Natürliche Person" ]
                    , option [ value "LegalEntity" ] [ text "Juristische Person / Gesellschaft" ]
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


rightToDeductInputTaxForm : RightToDeductInputTax -> Html Msg
rightToDeductInputTaxForm rightToDeductInputTax =
    form [ class "mb-3" ]
        [ div [ class "form-check" ]
            [ input [ class "form-check-input", id "rightToDeductInputTaxFormCheckbox", type_ "checkbox", value "", checked rightToDeductInputTax, onCheck RightToDeductInputTaxForm ]
                []
            , label [ class "form-check-label", for "rightToDeductInputTaxFormCheckbox" ]
                [ text "Vorsteuerabzugsberechtigung unserer Mandantschaft" ]
            ]
        ]


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

        labelTextGeneral : String
        labelTextGeneral =
            "Anrede im Brief"
    in
    form [ classes "mb-3 row g-3" ]
        [ div [ class "col-md-6" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6" ]
                    [ label [ class "form-label", for "opponenGreetingFormSwitchOpponentGreetingForm" ] [ text labelTextGeneral ]
                    , select
                        [ class "form-select"
                        , id "opponenGreetingFormSwitchOpponentGreetingForm"
                        , attribute "aria-label" labelTextGeneral
                        , onInput switchOpponentGreetingForm
                        ]
                        [ option [ value "GreetingSir", selected <| opponentGreetingCase opponentGreeting (GreetingSir "") ] [ text "Sehr geehrter Herr ...," ]
                        , option [ value "GreetingMadame", selected <| opponentGreetingCase opponentGreeting (GreetingMadame "") ] [ text "Sehr geehrte Frau ...," ]
                        , option [ value "GreetingCommon", selected <| opponentGreetingCase opponentGreeting GreetingCommon ] [ text "Sehr geehrte Damen und Herren," ]
                        ]
                    ]
                , case opponentGreeting of
                    GreetingSir txt ->
                        let
                            labelText =
                                "Name des Empfängers"
                        in
                        div [ class "col-md-6" ]
                            [ label [ class "form-label", for "opponentGreetingFormGreetingSir" ] [ text labelText ]
                            , input
                                [ class "form-control"
                                , id "opponentGreetingFormGreetingSir"
                                , type_ "text"
                                , placeholder labelText
                                , attribute "aria-label" labelText
                                , onInput OpponentGreetingFormGreetingSir
                                , value txt
                                ]
                                []
                            ]

                    GreetingMadame txt ->
                        let
                            labelText =
                                "Name des Empfängers"
                        in
                        div [ class "col-md-6" ]
                            [ label [ class "form-label", for "opponentGreetingFormGreetingMadame" ] [ text labelText ]
                            , input
                                [ class "form-control"
                                , id "opponentGreetingFormGreetingMadame"
                                , type_ "text"
                                , placeholder labelText
                                , attribute "aria-label" labelText
                                , onInput OpponentGreetingFormGreetingMadame
                                , value txt
                                ]
                                []
                            ]

                    GreetingCommon ->
                        div [ class "col-md-6" ] []
                ]
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
    let
        labelText : String
        labelText =
            "Rechtsgrund der Forderung"
    in
    form [ class "mb-3 row g-3" ]
        [ div [ class "col-md-6" ]
            [ label [ class "form-label", for "legalReasonForm" ] [ text labelText ]
            , textarea
                [ class "form-control"
                , id "legalReasonForm"
                , rows 2
                , placeholder labelText
                , attribute "aria-label" labelText
                , onInput LegalReasonForm
                , value legalReason
                ]
                []
            ]
        ]


amountForm : PrincipalAmount -> SumOfAmount -> PaymentToRepresentative -> Html Msg
amountForm principalAmount sumOfAmount paymentToRepresentative =
    let
        labelText1 : String
        labelText1 =
            "Hauptforderung in EUR"

        labelText2 : String
        labelText2 =
            "Gesamtforderung in EUR"

        labelText3 : String
        labelText3 =
            "Zahlung an Kanzlei"
    in
    form [ classes "mb-3 row g-3" ]
        [ div [ class "col-md-3" ]
            [ label [ class "form-label", for "principalAmountForm" ] [ text labelText1 ]
            , input
                [ class "form-control"
                , id "principalAmountForm"
                , type_ "text"
                , placeholder labelText1
                , attribute "aria-label" labelText1
                , onInput PrincipalAmountForm
                , value principalAmount
                ]
                []
            ]
        , div [ class "col-md-3" ]
            [ label [ class "form-label", for "sumOfAmountForm" ] [ text labelText2 ]
            , input
                [ class "form-control"
                , id "sumOfAmountForm"
                , type_ "text"
                , placeholder labelText2
                , attribute "aria-label" labelText2
                , onInput SumOfAmountForm
                , value sumOfAmount
                ]
                []
            ]
        , div [ class "col-md-3" ]
            [ div [ class "form-check" ]
                [ input
                    [ class "form-check-input"
                    , id "paymentToRepresentativeFormCheckbox"
                    , type_ "checkbox"
                    , value ""
                    , checked paymentToRepresentative
                    , onCheck PaymentToRepresentativeForm
                    ]
                    []
                , label [ class "form-check-label", for "paymentToRepresentativeFormCheckbox" ]
                    [ text labelText3 ]
                ]
            ]
        ]


delayForm : AgreementOfBeginningOfDelay -> TimeOfDelay -> DefaultInterest -> Html Msg
delayForm agreementOfBeginningOfDelay timeOfDelay defaultInterest =
    form [ classes "mb-3 row g-3" ]
        [ agreementOfBeginningOfDelayForm agreementOfBeginningOfDelay
        , timeOfDelayForm timeOfDelay
        , defaultInterestForm defaultInterest |> map DefaultInterestForm
        ]


agreementOfBeginningOfDelayForm : AgreementOfBeginningOfDelay -> Html Msg
agreementOfBeginningOfDelayForm agreementOfBeginningOfDelay =
    let
        labelText : String
        labelText =
            "Fälligkeit nach Rechnung in Tagen"
    in
    div [ class "col-md-3" ]
        [ label [ class "form-label", for "agreementOfBeginningOfDelayForm" ] [ text labelText ]
        , input
            [ class "form-control"
            , id "agreementOfBeginningOfDelayForm"
            , type_ "text"
            , placeholder labelText
            , attribute "aria-label" labelText
            , onInput AgreementOfBeginningOfDelayForm
            , value agreementOfBeginningOfDelay
            ]
            []
        ]


timeOfDelayForm : TimeOfDelay -> Html Msg
timeOfDelayForm timeOfDelay =
    let
        labelText : String
        labelText =
            "Beginn des Verzugs"
    in
    div [ class "col-md-3" ]
        [ label [ class "form-label", for "timeOfDelayForm" ] [ text labelText ]
        , input
            [ class "form-control"
            , id "timeOfDelayForm"
            , type_ "text"
            , placeholder labelText
            , attribute "aria-label" labelText
            , onInput TimeOfDelayForm
            , value timeOfDelay
            ]
            []
        ]


defaultInterestForm : DefaultInterest -> Html DefaultInterestForm
defaultInterestForm defaultInterest =
    let
        labelText1 : String
        labelText1 =
            "Verzugszinsen"

        labelText2 : String
        labelText2 =
            "Begründung für den höheren Verzugszins"
    in
    div [ class "col-md-6" ]
        [ div [ class "row" ]
            [ div [ class "col-md-6" ]
                [ label [ class "form-label", for "defaultInterestFormSwitchDefaultInterest" ] [ text labelText1 ]
                , select
                    [ class "form-select"
                    , id "defaultInterestFormSwitchDefaultInterest"
                    , attribute "aria-label" labelText1
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
                    div [ class "col-md-6" ]
                        [ label [ class "form-label", for "defaultInterestFormHigherDefaultInterest" ] [ text labelText2 ]
                        , input
                            [ class "form-control"
                            , id "defaultInterestFormHigherDefaultInterest"
                            , type_ "text"
                            , placeholder labelText2
                            , attribute "aria-label" labelText2
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



-- RESULT


result : Model -> Html Msg
result model =
    div [ class "col-8" ]
        [ div [ class "pt-5" ] (rubrum model.referenceNumber model.client)
        , p [] [ text <| greeting model.opponentGreeting ]
        , p [] [ text <| representation model.client ]
        , div [] (summary model.referenceNumber model.client model.paymentToRepresentative model.sumOfAmount)
        , p [] [ text "Im Einzelnen:" ]
        , p [] [ text <| claim model.client model.legalReason model.principalAmount ]
        , p [] [ text <| default model.client model.agreementOfBeginningOfDelay model.timeOfDelay ]
        , p [] [ text <| defaultInterestText model.defaultInterest ]
        , p [] [ text <| sumOfAmoutText model.sumOfAmount ]
        , p [] [ text <| requestForPayment model.client model.paymentToRepresentative ]
        , lawyersFees model.client model.principalAmount model.rightToDeductInputTax
        , p [] [ text <| judicialEnforcement model.client ]
        , p [] [ text "Die für uns zuständige Rechtsanwaltskammer ist die Rechtsanwaltskammer Sachsen, Glacisstraße 6, 01099 Dresden. Die E-Mail-Adresse der Rechtsanwaltskammer Sachsen lautet info@rak-sachsen.de." ]
        , p [] [ text "Mit freundlichen Grüßen" ]
        ]


rubrum : ReferenceNumber -> Client -> List (Html Msg)
rubrum referenceNumber client =
    let
        name : Name
        name =
            case client of
                NaturalPerson _ n _ ->
                    n

                LegalEntity _ n _ ->
                    n
    in
    [ p [] [ text <| "Aktenzeichen: " ++ referenceNumber ]
    , p [] [ strong [] [ text <| name ++ " ./. ..." ], br [] [], strong [] [ text "wegen ..." ] ]
    ]


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


summary : ReferenceNumber -> Client -> PaymentToRepresentative -> SumOfAmount -> List (Html Msg)
summary referenceNumber client paymentToRepresentative sumOfAmount =
    let
        clientName : Name
        clientName =
            case client of
                NaturalPerson _ n _ ->
                    n

                LegalEntity _ n _ ->
                    n
    in
    if paymentToRepresentative then
        [ p [] [ text "Zusammenfassung:" ]
        , p [] [ text "Bitte überweisen Sie aus den nachfolgenden Gründen binnen 10 Tagen nach Erhalt dieses Schreibens folgenden Betrag:" ]
        , ul [ class "list-without-marker" ]
            [ li [] [ text <| "Forderung und Rechtsanwaltskosten: EUR ... ← Hier Summe ausrechnen" ]
            , li [] [ text "IBAN: DE..." ]
            , li [] [ text <| "Verwendungszweck: " ++ clientName ++ ", Az. " ++ referenceNumber ]
            , li [] [ text "Kontoinhaber: GOB Legal Rechtsanwaltsgesellschaft mbH" ]
            ]
        ]

    else
        [ p [] [ text "Zusammenfassung:" ]
        , p [] [ text "Bitte überweisen Sie aus den nachfolgenden Gründen binnen 10 Tagen nach Erhalt dieses Schreibens folgende Beträge:" ]
        , ul [ class "list-without-marker" ]
            [ li [] [ text <| "Forderung: EUR " ++ sumOfAmount ]
            , li [] [ text "IBAN: ..." ]
            , li [] [ text "Verwendungszweck: Forderung gemäß Schreiben der GOB Legal Rechtsanwaltsgesellschaft vom ..." ]
            , li [] [ text <| "Kontoinhaber: " ++ clientName ]
            ]
        , ul [ class "list-without-marker" ]
            [ li [] [ text "Rechtsanwaltskosten: EUR ..." ]
            , li [] [ text "IBAN: DE..." ]
            , li [] [ text <| "Verwendungszweck: " ++ clientName ++ ", Az. " ++ referenceNumber ]
            , li [] [ text "Kontoinhaber: GOB Legal Rechtsanwaltsgesellschaft mbH" ]
            ]
        ]


claim : Client -> LegalReason -> PrincipalAmount -> String
claim client legalReason principalAmount =
    "Sie schulden " ++ clientDative client ++ " " ++ legalReason ++ " noch einen Betrag in Höhe von EUR " ++ principalAmount ++ "."


default : Client -> AgreementOfBeginningOfDelay -> TimeOfDelay -> String
default client agreementOfBeginningOfDelay timeOfDelay =
    "Vertraglich war vereinbart, dass Sie die Forderung "
        ++ clientGenitive client
        ++ " binnen "
        ++ agreementOfBeginningOfDelay
        ++ " Tagen nach Rechnungslegung begleichen. Sie sind deshalb seit dem "
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


sumOfAmoutText : SumOfAmount -> String
sumOfAmoutText sumOfAmount =
    "Die Gesamtforderung (ohne Rechtsanwaltskosten) beträgt EUR " ++ sumOfAmount ++ "."


requestForPayment : Client -> PaymentToRepresentative -> String
requestForPayment client paymentToRepresentative =
    "Namens "
        ++ clientGenitive client
        ++ " fordere ich Sie auf, den Betrag wie oben bezeichnet zu überweisen."
        ++ (if paymentToRepresentative then
                " Wir sind zum Empfang der Zahlung befugt."

            else
                ""
           )


lawyersFees : Client -> PrincipalAmount -> RightToDeductInputTax -> Html Msg
lawyersFees client principalAmount rightToDeductInputTax =
    div []
        [ p []
            [ text <|
                "Da Sie sich im Verzug befinden, schulden Sie auch die Freistellung "
                    ++ clientGenitive client
                    ++ " von den Kosten unserer Beauftragung. Diese Kosten berechnen sich nach dem Rechtsanwaltsvergütungsgesetz wie folgt:"
            ]
        , p [] [ text <| "Gegenstandswert: EUR " ++ principalAmount ]
        , div [ class "row" ]
            [ div [ classes "col-9 offset-1" ]
                [ table [ classes "table table-borderless table-sm align-top" ]
                    [ tbody []
                        ([ tr []
                            [ td [] [ text "0,9 bzw. 1,3 Geschäftsgebühr Nr. 2300 VV RVG:" ]
                            , td [ class "rvg-table-eur" ] [ text "EUR ..." ]
                            ]
                         , tr []
                            [ td [] [ text "Pauschale für Entgelte für Post- und Telekommunikationsdienstleistungen Nr. 7002 VV RVG:" ]
                            , td [] [ text "EUR ..." ]
                            ]
                         ]
                            ++ (if not rightToDeductInputTax then
                                    [ tr []
                                        [ td [] [ text "Umsatzsteuer 19 % Nr. 7008 VV RVG:" ]
                                        , td [] [ text "EUR ..." ]
                                        ]
                                    ]

                                else
                                    []
                               )
                        )
                    , tfoot []
                        [ tr [ class "rvg-table-sum" ]
                            [ td [] [ strong [] [ text "Summe:" ] ]
                            , td [] [ strong [] [ text "EUR ..." ] ]
                            ]
                        ]
                    ]
                ]
            ]
        , rightToDeductInputTaxText client rightToDeductInputTax
        , p []
            [ text <|
                "Sie können die Freistellung u. a. dadurch bewirken, dass Sie die Rechtsanwaltskosten an uns wie oben angegeben überweisen. "
                    ++ "Für die Freistellung setze ich Ihnen ebenfalls eine Frist von 10 Tagen nach Zugang dieses Schreibens. Sollte die Frist fruchtlos verstreichen, wird "
                    ++ clientNominative client
                    ++ " die Freistellung ablehnen und Ersatz in Geld verlangen."
            ]
        , p [] [ text "Weitere Inkassokosten werden derzeit nicht geltend gemacht." ]
        ]


rightToDeductInputTaxText : Client -> RightToDeductInputTax -> Html Msg
rightToDeductInputTaxText client rightToDeductInputTax =
    if not rightToDeductInputTax then
        p []
            [ text <|
                (clientNominative client
                    |> String.left 1
                    |> String.toUpper
                )
                    ++ (clientNominative client |> String.dropLeft 1)
                    ++ " ist zum Vorsteuerabzug nicht berechtigt."
            ]

    else
        div [] []


judicialEnforcement : Client -> String
judicialEnforcement client =
    "Sollte die vorgenannten Fristen fruchtlos verstreichen, werde ich "
        ++ clientDative client
        ++ " empfehlen, die Forderung gegen Sie gerichtlich durchzusetzen."



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
