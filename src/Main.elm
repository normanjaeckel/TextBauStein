module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { client : Client
    , opponentGreeting : OpponentGreeting
    , legalReason : String
    , defaultInterest : DefaultInterest
    }


init : Model
init =
    Model
        (NaturalPerson Male "Max Mustermann" "Musterstraße 1, 12345 Musterstadt")
        GreetingCommon
        "aus dem mit Ihnen geschlossenen Liefervertrag vom ... gemäß Rechnung Nr. ... vom ..."
        LegalDefaultInterest


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


type DefaultInterest
    = LegalDefaultInterest
    | HigherDefaultInterest String



-- UPDATE


type Msg
    = Foo


update : Msg -> Model -> Model
update _ model =
    model



-- VIEW


view : Model -> Html Msg
view model =
    div [ classes "container p-3 pb-5" ]
        [ main_ []
            [ h1 [] [ text "TextBauStein Mahnschreiben" ]
            , div [ class "mb-5" ]
                [ h2 [] [ text "Eingaben" ]
                , div [] []
                ]
            , div []
                [ h2 [] [ text "Ergebnis" ]
                , result model
                ]
            ]
        ]


result : Model -> Html Msg
result model =
    div [ class "col-8" ]
        [ p [ class "pt-3" ] (rubrum model.client)
        , p [] [ text <| greeting model.opponentGreeting ]
        , p [] [ text <| representation model.client ]
        , p [] [ text <| claim model ]
        , p [] [ text <| default model ]
        , p [] [ text <| defaultInterest model.defaultInterest ]
        , p [] [ text "Namens m. M. fordere ich Sie auf, den aus der Forderungsaufstellung ersichtlichen Gesamtbetrag in Höhe von EUR ... binnen 10 Tagen auf das Konto m. M. mit der IBAN ... zu überweisen." ]
        , p [] [ text "Da Sie sich im Verzug befinden, schulden Sie auch ..." ]
        , p [] [ text "RVG Tabelle" ]
        , p [] [ text "M. M. ist zum Vorsteuerabzug nicht berechtigt." ]
        , p [] [ text "Sie können die Freistellung ..." ]
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


claim : Model -> String
claim model =
    "Sie schulden " ++ clientDative model.client ++ " " ++ model.legalReason ++ " noch einen Betrag in Höhe von EUR ..."


default : Model -> String
default model =
    "Vertraglich war vereinbart, dass Sie die Forderung "
        ++ clientGenitive model.client
        ++ " binnen ... Tagen nach Rechnungslegung begleichen. Sie sind daher seit ... im Verzug."


defaultInterest : DefaultInterest -> String
defaultInterest defaultInterestValue =
    case defaultInterestValue of
        LegalDefaultInterest ->
            "Sie schulden daher zusätzlich Verzugszinsen in gesetzlicher Höhe. Die Zinsberechnung entnehmen Sie bitte der beiliegenden Forderungsaufstellung."

        HigherDefaultInterest t ->
            "Sie schulden daher zusätzlich Verzugszinsen. Der Zinssatz liegt über dem gesetzlichen Verzugszins, weil " ++ t ++ "."



-- HELPERS


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
