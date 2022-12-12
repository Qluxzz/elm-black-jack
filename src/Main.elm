module Main exposing (main)

import Browser
import Card exposing (Card, Suit(..), Value(..))
import Deck
import Hand
import Html
import Html.Attributes
import Html.Events
import Random
import Random.List



{- Black Jack

   Game loop:

   All players in the game has to at least bet the minimum of the table
   The players are dealed two cards, the dealer is dealed two cards, but one is hidden

   Each player is given the choice to take another card or stand,
   When all players are standing or busted, the dealer flips the hidden card.

   If the dealers total sum is below the table minimum (often 16), the dealer takes cards until he reaches 16 or is bust.

   If the dealer busts, all players not busted win.
-}


type alias Dollars =
    Int


type Msg
    = ShuffleDeck
    | ShuffledDeck Deck.Deck
      -- Betting
    | Bet Dollars
      -- Hit or Stand
    | TakeCard
    | Stand
    | Split
    | DoubleDown


type alias Dealer =
    List Card


type alias Model =
    { state : GameState
    , deck : Deck.Deck
    , dealer : Dealer
    , players : List Player
    }


type alias Player =
    { hands : List (List Card)
    , money : Int
    , currentBet : Int
    , type_ : PlayerType
    }


type PlayerType
    = Real
    | AI -- TODO: Strategies/Personalities



-- Should this be split up into one "game loop" per state?
-- That way the HitOrStand game could have it's own messages, and doesn't need to know about other game state messages


type GameState
    = ShuffleCards -- Only done at start and if not enough cards left for a new round, TODO: Implement cutting the deck?
    | Betting
    | Deal
    | HitOrStand
    | Result


init : ( Model, Cmd Msg )
init =
    ( { deck = []
      , dealer = []
      , players =
            [ { type_ = Real
              , money = 1000
              , currentBet = 0
              , hands = []
              }
            ]
      , state = ShuffleCards
      }
    , shuffleDeck (Deck.decks 4)
    )


shuffleDeck : Deck.Deck -> Cmd Msg
shuffleDeck deck =
    Random.generate ShuffledDeck (Random.List.shuffle deck)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = \model -> view model
        , update = update
        , subscriptions = \_ -> Sub.none
        }


deal : Deck.Deck -> List Player -> ( Deck.Deck, Dealer, List Player )
deal deck players =
    let
        ( updatedDeck, updatedPlayers ) =
            List.foldr
                (\player ->
                    \( d, p ) ->
                        let
                            ( cards, d2 ) =
                                Deck.takeCards d 2

                            updatedPlayer : Player
                            updatedPlayer =
                                { player | hands = [ cards ] }
                        in
                        ( d2, updatedPlayer :: p )
                )
                ( deck, [] )
                players

        ( updatedDeck2, updatedDealer ) =
            let
                ( cards, d ) =
                    Deck.takeCards updatedDeck 2
            in
            ( d, cards )
    in
    ( updatedDeck2, updatedDealer, updatedPlayers )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleDeck ->
            ( model, Cmd.none )

        ShuffledDeck deck ->
            ( { model | deck = deck, state = Betting }, Cmd.none )

        {- Betting -}
        -- Need to know which player
        -- Remove bet amount from player money
        -- Set amount as bet
        -- Continue to next player
        Bet amount ->
            let
                ( deck, dealer, players ) =
                    deal model.deck model.players
            in
            ( { model
                | state = HitOrStand
                , deck = deck
                , dealer = dealer
                , players = players
              }
            , Cmd.none
            )

        {- Hit or Stand -}
        -- Add card to player hand
        -- Need to know which hand, if player has multiple
        TakeCard ->
            ( model, Cmd.none )

        -- Continue to next player
        Stand ->
            ( model, Cmd.none )

        -- Split current player hands into two, allow player to take a card/stand/split/doubledown on each hand
        Split ->
            ( model, Cmd.none )

        -- Double bet and take another card
        DoubleDown ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ dealerView model.dealer
        , Html.div [ Html.Attributes.style "display" "flex" ]
            (List.map (playerView model.state) model.players)
        ]


dealerView : Dealer -> Html.Html Msg
dealerView dealer =
    -- TODO: While we're not in the results state, the second card of the dealer should be hidden
    Html.div [] (List.map cardView dealer ++ [ Html.p [] [ Html.text (handValue dealer) ] ])


playerView : GameState -> Player -> Html.Html Msg
playerView state player =
    Html.div []
        [ Html.p []
            [ Html.text ("$" ++ String.fromInt player.money) ]
        , case state of
            Betting ->
                Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
                    (List.map
                        (\amount ->
                            Html.button [ Html.Events.onClick (Bet 1) ] [ Html.text ("Bet $" ++ String.fromInt amount) ]
                        )
                        [ 1, 5, 10, 25, 50 ]
                    )

            HitOrStand ->
                Html.div []
                    (List.map
                        handView
                        player.hands
                    )

            _ ->
                Html.text ""
        ]


canSplit : Hand.Hand -> Bool
canSplit hand =
    case hand of
        first :: second :: _ ->
            first.value == second.value

        _ ->
            False


handValue : Hand.Hand -> String
handValue hand =
    hand
        |> Hand.value
        |> (\( v1, v2 ) ->
                if v1 == v2 then
                    String.fromInt v1

                else
                    String.fromInt v1 ++ "/" ++ String.fromInt v2
           )


handView : Hand.Hand -> Html.Html Msg
handView hand =
    Html.div []
        (List.map cardView hand
            ++ [ Html.p [] [ Html.text (handValue hand) ]
               , Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
                    [ Html.button [ Html.Events.onClick TakeCard ] [ Html.text "Take a card" ]
                    , Html.button [ Html.Events.onClick Stand ] [ Html.text "Stand" ]
                    , if canSplit hand then
                        Html.button [ Html.Events.onClick Split ] [ Html.text "Split" ]

                      else
                        Html.text ""
                    , if List.length hand == 2 then
                        Html.button [ Html.Events.onClick DoubleDown ] [ Html.text "Double down" ]

                      else
                        Html.text ""
                    ]
               ]
        )


cardView : Card -> Html.Html msg
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
