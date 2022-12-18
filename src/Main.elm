module Main exposing (main)

import Browser
import Card exposing (Card, Suit(..), Value(..))
import Deck
import Dict exposing (Dict)
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


type alias PlayerId =
    Int


type alias HandId =
    Int


type alias Dollars =
    Int


type Msg
    = ShuffleDeck
    | ShuffledDeck Deck.Deck
      -- Betting
    | Bet Dollars
      -- Deal
    | Deal
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
    , players : Dict PlayerId Player
    , currentPlayer : PlayerId
    }


type HandState
    = Playing
    | Busted


type alias Hand =
    { cards : List Card
    , bet : Int
    , state : HandState
    }


type alias Player =
    { hands : Dict HandId Hand
    , money : Int
    , type_ : PlayerType
    , selectedHand : HandId
    }


type PlayerType
    = Real
    | AI -- TODO: Strategies/Personalities


type GameState
    = ShuffleCards -- Only done at start and if not enough cards left for a new round, TODO: Implement cutting the deck?
    | Betting
    | Dealing
    | HitOrStand
    | Result


init : ( Model, Cmd Msg )
init =
    ( { deck = []
      , dealer = []
      , players =
            Dict.fromList
                [ ( 0
                  , { type_ = AI
                    , money = 5
                    , hands = Dict.empty
                    , selectedHand = 0
                    }
                  )
                ]
      , currentPlayer = 0
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatePlayer =
            updatePlayer_ model.players model.currentPlayer

        changeState =
            model.currentPlayer + 1 == Dict.size model.players

        nextPlayer =
            if changeState then
                0

            else
                model.currentPlayer + 1
    in
    case msg of
        ShuffleDeck ->
            ( model, Cmd.none )

        ShuffledDeck deck ->
            ( { model | deck = deck, state = Betting }, Cmd.none )

        {- Betting -}
        Bet amount ->
            ( { model
                | state =
                    if changeState then
                        Dealing

                    else
                        model.state
                , players = updatePlayer (\p -> { p | hands = Dict.fromList [ ( 0, { bet = amount, cards = [], state = Playing } ) ], money = p.money - amount })
                , currentPlayer = nextPlayer
              }
            , Cmd.none
            )

        {- DEALING -}
        Deal ->
            let
                ( cards, deck ) =
                    Deck.takeCards model.deck 2
            in
            ( { model
                | deck = deck
                , players =
                    updatePlayer
                        (\p -> { p | hands = Dict.update p.selectedHand (\h -> h |> Maybe.map (\hand -> { hand | cards = cards })) p.hands })
                , currentPlayer = nextPlayer
                , state =
                    if changeState then
                        HitOrStand

                    else
                        model.state
              }
            , Cmd.none
            )

        {- Hit or Stand -}
        -- Add card to player hand
        -- Need to know which hand, if player has multiple
        TakeCard ->
            ( model, Cmd.none )

        -- Continue to next player
        -- Stand on which hand?
        Stand ->
            ( { model
                | currentPlayer = nextPlayer
                , state =
                    if changeState then
                        Result

                    else
                        model.state
              }
            , Cmd.none
            )

        -- Split current player hands into two, allow player to take a card/stand/split/doubledown on each hand
        Split ->
            let
                currentBet =
                    model.players

                updatedPlayers =
                    updatePlayer
                        (\p ->
                            { p
                                | hands =
                                    Dict.empty
                            }
                        )
            in
            ( { model | players = updatedPlayers }, Cmd.none )

        -- Double bet and take another card
        DoubleDown ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ dealerView model.dealer
        , Html.div [ Html.Attributes.style "display" "flex" ]
            (List.map (playerView model.state) (Dict.values model.players))
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
                            Html.button [ Html.Events.onClick (Bet amount), Html.Attributes.disabled (player.money < amount) ] [ Html.text ("Bet $" ++ String.fromInt amount) ]
                        )
                        [ 1, 5, 10, 25, 50 ]
                    )

            Dealing ->
                Html.div []
                    [ Html.button [ Html.Events.onClick Deal ] [ Html.text "Deal!" ]
                    ]

            HitOrStand ->
                Html.div []
                    (List.map
                        (handView player.money)
                        (Dict.values player.hands)
                    )

            _ ->
                Html.text ""
        ]


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


handView : Dollars -> Hand -> Html.Html Msg
handView money { cards, bet } =
    Html.div []
        (List.map cardView cards
            ++ [ Html.p [] [ Html.text (handValue cards) ]
               , Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
                    [ Html.button [ Html.Events.onClick TakeCard ] [ Html.text "Take a card" ]
                    , Html.button [ Html.Events.onClick Stand ] [ Html.text "Stand" ]
                    , if canSplit cards && money > bet then
                        Html.button [ Html.Events.onClick Split ] [ Html.text "Split" ]

                      else
                        Html.text ""
                    , if List.length cards == 2 then
                        Html.button [ Html.Events.onClick DoubleDown ] [ Html.text "Double down" ]

                      else
                        Html.text ""
                    ]
               ]
        )


cardView : Card -> Html.Html msg
cardView card =
    Html.div [] [ suiteView card, Html.span [] [ Html.text (Card.valueString card) ] ]



-- HELPER FUNCTIONS


updatePlayer_ :
    Dict PlayerId Player
    -> PlayerId
    -> (Player -> Player)
    -> Dict PlayerId Player
updatePlayer_ players index new =
    Dict.update
        index
        (Maybe.map new)
        players


canSplit : Hand.Hand -> Bool
canSplit hand =
    case hand of
        first :: second :: _ ->
            first.value == second.value

        _ ->
            False


suiteView : Card -> Html.Html msg
suiteView card =
    let
        span : List (Html.Attribute msg) -> String -> Html.Html msg
        span attr s =
            Html.span attr [ Html.text s ]

        black =
            span []

        red =
            span [ Html.Attributes.style "color" "red" ]

        symbol =
            Card.suiteString card

        color =
            case card.suite of
                Spades ->
                    black

                Diamonds ->
                    red

                Hearts ->
                    red

                Clubs ->
                    black
    in
    color symbol
