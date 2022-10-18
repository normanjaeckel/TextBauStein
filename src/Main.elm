module Main exposing (main)

import Browser
import Html exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { foo : String
    }


init : Model
init =
    Model ""



-- UPDATE


type Msg
    = Foo


update : Msg -> Model -> Model
update _ model =
    model



-- VIEW


view : Model -> Html Msg
view _ =
    div [] []
