module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Mahnschreiben
import Risikorechner


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { page : Page
    , mahnschreibenModel : Mahnschreiben.Model
    , risikorechnerModel : Risikorechner.Model
    }


type Page
    = Home
    | MahnschreibenPage
    | RisikorechnerPage


init : Model
init =
    Model
        Home
        Mahnschreiben.init
        Risikorechner.init



-- UPDATE


type Msg
    = MahnschreibenMsg Mahnschreiben.Msg
    | RisikorechnerMsg Risikorechner.Msg
    | ChangePage Page


update : Msg -> Model -> Model
update msg model =
    case msg of
        MahnschreibenMsg innerMsg ->
            { model | mahnschreibenModel = Mahnschreiben.update innerMsg model.mahnschreibenModel }

        RisikorechnerMsg innerMsg ->
            { model | risikorechnerModel = Risikorechner.update innerMsg model.risikorechnerModel }

        ChangePage p ->
            { model | page = p }



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Home ->
            homeView

        MahnschreibenPage ->
            Mahnschreiben.view model.mahnschreibenModel |> Html.map MahnschreibenMsg

        RisikorechnerPage ->
            Risikorechner.view model.risikorechnerModel |> Html.map RisikorechnerMsg


homeView : Html Msg
homeView =
    div [ class "container", class "p-3", class "pb-5" ]
        [ main_ []
            [ h1 [ class "mb-3" ] [ text "TextBauStein" ]
            , div [ class "mb-5" ]
                [ button
                    [ type_ "button"
                    , class "btn"
                    , class "btn-primary"
                    , onClick <| ChangePage MahnschreibenPage
                    ]
                    [ text "Mahnschreiben" ]
                , button
                    [ type_ "button"
                    , class "btn"
                    , class "btn-primary"
                    , class "ms-2"
                    , onClick <| ChangePage RisikorechnerPage
                    ]
                    [ text "Risikorechner" ]
                ]
            ]
        ]
