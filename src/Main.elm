module Main exposing (main)

import Browser
import Card exposing (Card, Suit(..), Value(..))
import Deck
import Html
import Html.Attributes
import Html.Events
import Random
import Random.List


type Msg
    = ShuffleDeck
    | ShuffledDeck Deck.Deck
    | TakeCard


type alias Model =
    { deck : Deck.Deck
    , hand : List Card
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { deck = Deck.decks 4, hand = [] }, Cmd.none )
        , view = \model -> view model
        , update = update
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleDeck ->
            ( model, Random.generate ShuffledDeck (Random.List.shuffle model.deck) )

        ShuffledDeck cards ->
            ( { model | deck = cards }, Cmd.none )

        TakeCard ->
            let
                ( card, updatedDeck ) =
                    Deck.takeCard model.deck
            in
            ( { model | deck = updatedDeck, hand = card :: model.hand }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.button [ Html.Events.onClick ShuffleDeck ] [ Html.text "Shuffle cards!" ]
        , Html.button [ Html.Events.onClick TakeCard ] [ Html.text "Take card!" ]
        , Html.div [ Html.Attributes.style "display" "flex" ]
            [ viewDeck model.deck
            , viewDeck model.hand
            ]
        ]


viewDeck : Deck.Deck -> Html.Html Msg
viewDeck cards =
    Html.div []
        (List.map
            cardView
            cards
        )


cardView : Card -> Html.Html Msg
cardView card =
    let
        symbol : Card -> Html.Html msg
        symbol { suite } =
            let
                black s =
                    Html.span [] [ Html.text s ]

                red s =
                    Html.span [ Html.Attributes.style "color" "red" ] [ Html.text s ]
            in
            case suite of
                Clubs ->
                    black "♣"

                Diamonds ->
                    red "♦"

                Hearts ->
                    red "♥"

                Spades ->
                    black "♠"

        value : Card -> String
        value c =
            case c.value of
                Ace ->
                    "A"

                Two ->
                    "2"

                Three ->
                    "3"

                Four ->
                    "4"

                Five ->
                    "5"

                Six ->
                    "6"

                Seven ->
                    "7"

                Eight ->
                    "8"

                Nine ->
                    "9"

                Ten ->
                    "10"

                Jack ->
                    "J"

                Queen ->
                    "Q"

                King ->
                    "K"
    in
    Html.div [] [ symbol card, Html.span [] [ Html.text (value card) ] ]
