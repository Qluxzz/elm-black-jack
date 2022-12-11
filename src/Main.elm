module Main exposing (main)

import Browser
import Html


type Msg
    = NoOp


type alias Model =
    {}


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( {}, Cmd.none )
        , view = \model -> view model
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div [] [ Html.text "Hello" ]
