module Main exposing (main)

import Browser
import Card exposing (Card, Suit(..), Value(..))
import Deck
import Dict exposing (Dict)
import Hand
import Html
import Html.Attributes
import Html.Events
import Process
import Random
import Random.List
import Task



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
    | DealerTakesCard
      -- Hit or Stand
    | TakeCard
    | Stand
    | Split
    | DoubleDown
    | NextRound


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
    | Standing
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


initalState : Model
initalState =
    { deck = []
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


{-| Helper method to start with a deterministic deck
-}
initWithDeck : Deck.Deck -> ( Model, Cmd Msg )
initWithDeck deck =
    ( { initalState | deck = deck, state = Betting }, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( initalState, shuffleDeck (Deck.decks 4) )


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

        allHandsHaveCond players cond =
            players
                |> Dict.values
                |> List.all
                    (\p ->
                        p.hands
                            |> Dict.values
                            |> List.all (\h -> cond h)
                    )
    in
    case msg of
        ShuffleDeck ->
            ( model, Cmd.none )

        ShuffledDeck deck ->
            ( { model | deck = deck, state = Betting }, Cmd.none )

        {- Betting -}
        Bet amount ->
            let
                updatedPlayers =
                    updatePlayer (\p -> { p | hands = Dict.fromList [ ( 0, { bet = amount, cards = [], state = Playing } ) ], money = p.money - amount })

                allHandsHaveBet =
                    allHandsHaveCond updatedPlayers (\h -> h.bet /= 0)
            in
            ( { model
                | state =
                    if allHandsHaveBet then
                        Dealing

                    else
                        model.state
                , players = updatedPlayers
                , currentPlayer =
                    if allHandsHaveBet then
                        0

                    else
                        model.currentPlayer + 1
              }
            , if allHandsHaveBet then
                Process.sleep 100 |> Task.perform (\_ -> Deal)

              else
                Cmd.none
            )

        {- DEALING -}
        Deal ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck

                updatedPlayers =
                    updatePlayer
                        (\p ->
                            { p
                                | hands = updateHand p.hands p.selectedHand (\h -> { h | cards = h.cards ++ cards })
                            }
                        )

                currentHandHasTwoCards =
                    Dict.get model.currentPlayer updatedPlayers
                        |> Maybe.andThen (\p -> Dict.get p.selectedHand p.hands |> Maybe.map (\h -> List.length h.cards == 2))
                        |> Maybe.withDefault False

                allHandsHaveOneCard =
                    allHandsHaveCond updatedPlayers (\h -> List.length h.cards == 1)
                        |> Debug.log "All players have one cards"

                allHandsHaveTwoCards =
                    allHandsHaveCond updatedPlayers (\h -> List.length h.cards == 2)
                        |> Debug.log "All players have two cards"
            in
            ( { model
                | deck = deck
                , players = updatedPlayers
                , currentPlayer =
                    if allHandsHaveTwoCards then
                        0

                    else if currentHandHasTwoCards then
                        model.currentPlayer + 1

                    else
                        model.currentPlayer
                , state =
                    if allHandsHaveTwoCards then
                        HitOrStand

                    else
                        model.state
              }
            , if allHandsHaveOneCard then
                Cmd.batch
                    [ Process.sleep 1000 |> Task.perform (\_ -> DealerTakesCard)
                    , Process.sleep 1000 |> Task.perform (\_ -> Deal)
                    ]

              else if allHandsHaveTwoCards then
                Process.sleep 1000 |> Task.perform (\_ -> DealerTakesCard)

              else
                Process.sleep 1000 |> Task.perform (\_ -> Deal)
            )

        DealerTakesCard ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck
            in
            ( { model | dealer = model.dealer ++ cards, deck = deck }, Cmd.none )

        {- Hit or Stand -}
        -- Add card to player hand
        -- Need to know which hand, if player has multiple
        TakeCard ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck

                updatedPlayers =
                    updatePlayer
                        (\p ->
                            { p
                                | hands =
                                    updateHand p.hands
                                        p.selectedHand
                                        (\h ->
                                            { h
                                                | cards = h.cards ++ cards
                                                , state =
                                                    if Tuple.first (Hand.value (h.cards ++ cards)) > 21 then
                                                        Busted

                                                    else
                                                        Playing
                                            }
                                        )
                            }
                        )

                allPlayersStandingOrBusted =
                    allHandsHaveCond updatedPlayers (\h -> List.member h.state [ Standing, Busted ])
                        |> Debug.log "All players standing or busted"

                dealerHandValue =
                    Hand.value model.dealer

                dealerHasReachedLimit =
                    Tuple.first dealerHandValue >= 17

                dealerHasBust =
                    Tuple.first dealerHandValue > 21
            in
            ( { model
                | deck = deck
                , players = updatedPlayers
                , state =
                    if allPlayersStandingOrBusted && xor dealerHasReachedLimit dealerHasBust then
                        Result

                    else
                        model.state
              }
            , if dealerHasReachedLimit then
                Process.sleep 1000 |> Task.perform (\_ -> DealerTakesCard)

              else
                Cmd.none
            )

        -- Continue to next player
        -- Stand on which hand?
        Stand ->
            let
                updatedPlayers =
                    updatePlayer
                        (\p ->
                            let
                                nextHand =
                                    if p.selectedHand + 1 == Dict.size p.hands then
                                        0

                                    else
                                        p.selectedHand + 1
                            in
                            { p
                                | hands = updateHand p.hands p.selectedHand (\h -> { h | state = Standing })
                                , selectedHand = nextHand
                            }
                        )

                allPlayersStandingOrBusted =
                    allHandsHaveCond updatedPlayers (\h -> List.member h.state [ Standing, Busted ])
                        |> Debug.log "All players standing or busted"

                dealerHandValue =
                    Hand.value model.dealer

                dealerHasReachedLimit =
                    Tuple.first dealerHandValue >= 17

                dealerHasBust =
                    Tuple.first dealerHandValue > 21

                newState =
                    if allPlayersStandingOrBusted && xor dealerHasReachedLimit dealerHasBust then
                        Result

                    else
                        model.state
            in
            ( { model
                | players = updatedPlayers
                , state = newState
              }
            , if not dealerHasReachedLimit then
                Process.sleep 1000 |> Task.perform (\_ -> DealerTakesCard)

              else
                Cmd.none
            )

        -- Split current player hands into two, allow player to take a card/stand/split/doubledown on each hand
        Split ->
            ( model, Cmd.none )

        -- Double bet and take another card
        DoubleDown ->
            let
                currentPlayer =
                    Dict.get model.currentPlayer model.players

                currentBet =
                    currentPlayer
                        |> Maybe.andThen (\p -> Dict.get p.selectedHand p.hands |> Maybe.map (\h -> h.bet))

                ( cards, deck ) =
                    Deck.takeCard model.deck

                updatedPlayers =
                    updatePlayer
                        (\p ->
                            let
                                nextHand =
                                    if p.selectedHand + 1 == Dict.size p.hands then
                                        0

                                    else
                                        p.selectedHand + 1
                            in
                            { p
                                | hands =
                                    updateHand p.hands
                                        p.selectedHand
                                        (\h ->
                                            { h
                                                | state = Standing
                                                , bet = h.bet * 2
                                                , cards = h.cards ++ cards
                                            }
                                        )
                                , money = p.money - (Maybe.map (\b -> b * 2) currentBet |> Maybe.withDefault 0)
                                , selectedHand = nextHand
                            }
                        )
            in
            ( { model | players = updatedPlayers, deck = deck }, Cmd.none )

        NextRound ->
            let
                cleared =
                    Dict.map (\_ -> \p -> { p | hands = Dict.empty }) model.players

                newState =
                    initalState
            in
            ( { newState | state = Betting, deck = model.deck, players = cleared }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        ((dealerView model.dealer model.state
            :: List.map (playerView model.state) (Dict.values model.players)
         )
            ++ (if model.state == Result then
                    [ Html.div [ Html.Attributes.class "result" ]
                        [ Html.button [ Html.Events.onClick NextRound ] [ Html.text "Continue?" ]
                        ]
                    ]

                else
                    [ Html.text "Game ongoing" ]
               )
        )


cardView : Bool -> Card -> Html.Html msg
cardView hidden card =
    Html.div [ Html.Attributes.class "card", Html.Attributes.classList [ ( "hidden", hidden ) ] ]
        [ Html.div [ Html.Attributes.class "color", Html.Attributes.class (Card.suiteToCssClass card) ] []
        , Html.div [ Html.Attributes.class "value", Html.Attributes.class (Card.valueToCssClass card) ] []
        ]


dealerView : Dealer -> GameState -> Html.Html Msg
dealerView dealer state =
    if state == Result || List.length dealer > 2 then
        Html.div []
            [ Html.div [ Html.Attributes.class "cards" ] (List.map (cardView False) dealer)
            , Html.p [] [ Html.text (handValue dealer) ]
            ]

    else
        case dealer of
            first :: [] ->
                Html.div []
                    [ Html.div [ Html.Attributes.class "cards" ]
                        [ cardView False first
                        ]
                    , Html.p [] [ Html.text (handValue [ first ]) ]
                    ]

            first :: second :: rest ->
                Html.div []
                    [ Html.div [ Html.Attributes.class "cards" ]
                        ([ cardView False first
                         , cardView (state /= Result) second
                         ]
                            ++ List.map (cardView False) rest
                        )
                    , Html.p [] [ Html.text (handValue [ first ]) ]
                    ]

            [] ->
                Html.p [] [ Html.text "Dealer has no cards yet" ]


playerView : GameState -> Player -> Html.Html Msg
playerView state player =
    Html.div []
        [ Html.p []
            [ Html.text ("$" ++ String.fromInt player.money) ]
        , if state /= Betting then
            Html.div []
                (List.map
                    (handView player.money)
                    (Dict.values player.hands)
                )

          else
            Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
                (List.map
                    (\amount ->
                        Html.button [ Html.Events.onClick (Bet amount), Html.Attributes.disabled (player.money < amount) ] [ Html.text ("Bet $" ++ String.fromInt amount) ]
                    )
                    [ 1, 5, 10, 25, 50 ]
                )
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
handView money { cards, bet, state } =
    Html.div []
        [ Html.div []
            [ Html.div [ Html.Attributes.class "cards" ]
                (List.map (cardView False) cards)
            , Html.p [] [ Html.text (handValue cards) ]
            ]
        , Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
            [ Html.button [ Html.Events.onClick TakeCard, Html.Attributes.disabled (state /= Playing) ] [ Html.text "Take a card" ]
            , Html.button [ Html.Events.onClick Stand, Html.Attributes.disabled (state /= Playing) ] [ Html.text "Stand" ]
            , if canSplit cards && money > bet then
                Html.button [ Html.Events.onClick Split, Html.Attributes.disabled (state /= Playing) ] [ Html.text "Split" ]

              else
                Html.text ""
            , if List.length cards == 2 then
                Html.button
                    [ Html.Events.onClick DoubleDown
                    , Html.Attributes.disabled ((state /= Playing) || bet * 2 > money)
                    ]
                    [ Html.text "Double down" ]

              else
                Html.text ""
            ]
        ]



-- HELPER FUNCTIONS


updateHand :
    Dict HandId Hand
    -> HandId
    -> (Hand -> Hand)
    -> Dict HandId Hand
updateHand hands index new =
    Dict.update
        index
        (Maybe.map new)
        hands


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
        [ first, second ] ->
            first.value == second.value

        _ ->
            False
