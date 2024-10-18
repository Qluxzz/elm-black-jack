module Main exposing (main)

import Browser
import Card exposing (Card, Suit(..), Value(..))
import Deck
import Hand
import Html
import Html.Attributes
import Html.Events
import Platform.Cmd as Cmd
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

   If the dealers total sum is below the table minimum (often 17), the dealer takes cards until he reaches 17 or is bust.

   If the dealer busts, all players not busted win.
-}


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
      -- All players are now standing or have busted
    | DealerFinish
    | Winnings
      -- Toast
    | ShowToast String
    | ClearToast


type alias Dealer =
    List Card


type alias Model =
    { state : GameState
    , deck : Deck.Deck
    , dealer : Dealer
    , players : ( Player, List Player )
    , toast : Maybe String
    }


type HandState
    = Playing
    | Standing
    | Busted


type alias Hand =
    { cards : List Card
    , bet : Int
    , state : HandState
    , order : Int
    }


type alias Player =
    { hands : ( Hand, List Hand )
    , money : Int
    , type_ : PlayerType
    , order : Int
    }


type PlayerType
    = Real
    | AI -- TODO: Strategies/Personalities


type GameState
    = ShuffleCards -- Only done at start and if not enough cards left for a new round, TODO: Implement cutting the deck?
    | Betting
    | Dealing
    | HitOrStand
    | DealerFinishes
    | Result


initalState : Model
initalState =
    { deck = []
    , dealer = []
    , players =
        ( { type_ = AI
          , money = 50
          , hands = ( { cards = [], bet = 0, state = Playing, order = 0 }, [] )
          , order = 0
          }
        , []
        )
    , state = ShuffleCards
    , toast = Nothing
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
        currentPlayer : Player
        currentPlayer =
            Tuple.first model.players

        players : List Player
        players =
            Tuple.second model.players

        allPlayersHaveCond : ( Player, List Player ) -> (Hand -> Bool) -> Bool
        allPlayersHaveCond p cond =
            p |> allPlayers |> allHands |> List.all (\h -> cond h)
    in
    case msg of
        ShuffleDeck ->
            ( model, Cmd.none )

        ShuffledDeck deck ->
            ( { model | deck = deck, state = Betting }, Cmd.none )

        {- Betting -}
        Bet amount ->
            let
                updatedPlayer =
                    { currentPlayer | money = currentPlayer.money - amount, hands = updateCurrentHand currentPlayer.hands (\h -> { h | bet = amount }) }

                allHandsHaveBet =
                    allHandsHaveCond updatedPlayer (\h -> h.bet /= 0)
            in
            ( { model
                | state =
                    if allHandsHaveBet then
                        Dealing

                    else
                        model.state
                , players =
                    if allHandsHaveBet then
                        continueToNextPlayer updatedPlayer players

                    else
                        ( updatedPlayer, players )
              }
            , if allHandsHaveBet then
                Process.sleep 1000 |> Task.perform (\_ -> Deal)

              else
                Cmd.none
            )

        {- DEALING -}
        Deal ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck

                ( currentHand, rest ) =
                    currentPlayer.hands

                updatedCards =
                    currentHand.cards ++ cards

                state =
                    case Basics.compare (Hand.largestValue updatedCards) 21 of
                        GT ->
                            Busted

                        EQ ->
                            Standing

                        LT ->
                            Playing

                updatedCurrentHand =
                    { currentHand | cards = updatedCards, state = state }

                updatedPlayer =
                    { currentPlayer
                        | hands =
                            (if state == Busted then
                                continueToNextHand

                             else
                                identity
                            )
                                ( updatedCurrentHand, rest )
                    }

                currentHandHasTwoCards =
                    currentPlayer.hands
                        |> Tuple.first
                        |> (\selectedHand ->
                                List.length selectedHand.cards == 2
                           )

                updatedPlayers =
                    if currentHandHasTwoCards then
                        continueToNextPlayer updatedPlayer players

                    else
                        ( updatedPlayer, players )

                allPlayersHaveOneCard =
                    allPlayersHaveCond updatedPlayers (\h -> List.length h.cards == 1)

                allHandsHaveTwoCards =
                    allPlayersHaveCond updatedPlayers (\h -> List.length h.cards == 2)

                allPlayersStandingOrBusted =
                    allPlayersHaveCond updatedPlayers (\h -> List.member h.state [ Standing, Busted ])
            in
            ( { model
                | deck = deck
                , players = updatedPlayers
                , state =
                    if allHandsHaveTwoCards then
                        HitOrStand

                    else
                        model.state
              }
            , if allPlayersHaveOneCard then
                Process.sleep 1000 |> Task.perform (\_ -> DealerTakesCard)

              else if allHandsHaveTwoCards then
                Process.sleep 1000 |> Task.perform (\_ -> DealerTakesCard)

              else if allPlayersStandingOrBusted then
                Process.sleep 1000 |> Task.perform (\_ -> DealerFinish)

              else
                Process.sleep 1000 |> Task.perform (\_ -> Deal)
            )
                |> toastIf (state == Standing) "Black Jack!"

        DealerTakesCard ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck

                updatedHand =
                    model.dealer ++ cards

                hasTwoCards =
                    List.length updatedHand == 2

                allPlayersStandingOrBusted =
                    allPlayersHaveCond model.players (\h -> List.member h.state [ Standing, Busted ])
            in
            ( { model
                | dealer = updatedHand
                , state =
                    if hasTwoCards then
                        HitOrStand

                    else
                        model.state
                , deck = deck
              }
            , if allPlayersStandingOrBusted then
                Process.sleep 1000 |> Task.perform (\_ -> DealerFinish)

              else if hasTwoCards then
                Cmd.none

              else
                Process.sleep 1000 |> Task.perform (\_ -> Deal)
            )

        {- Hit or Stand -}
        -- Add card to player hand
        TakeCard ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck

                ( currentHand, rest ) =
                    currentPlayer.hands

                updatedCards =
                    currentHand.cards ++ cards

                state =
                    case Basics.compare (Hand.largestValue updatedCards) 21 of
                        GT ->
                            -- TODO: Switch to next hand
                            Busted

                        EQ ->
                            -- TODO: Show toast that you got black jack
                            Standing

                        LT ->
                            Playing

                updatedCurrentHand =
                    { currentHand | cards = updatedCards, state = state }

                updatedPlayer =
                    { currentPlayer
                        | hands =
                            (if state == Busted then
                                continueToNextHand

                             else
                                identity
                            )
                                ( updatedCurrentHand, rest )
                    }

                updatedPlayers =
                    ( updatedPlayer, players )

                allPlayersStandingOrBusted =
                    allPlayersHaveCond updatedPlayers (\h -> List.member h.state [ Standing, Busted ])
            in
            ( { model
                | deck = deck
                , players = updatedPlayers
                , state =
                    if allPlayersStandingOrBusted then
                        DealerFinishes

                    else
                        model.state
              }
            , if allPlayersStandingOrBusted then
                Process.sleep 1000 |> Task.perform (\_ -> DealerFinish)

              else
                Cmd.none
            )
                |> toastIf (state == Standing) "Black Jack!"
                |> toastIf (state == Busted) "Bust!"

        -- Continue to next player
        -- Stand on which hand?
        Stand ->
            let
                updatedPlayer =
                    { currentPlayer
                        | hands = continueToNextHand <| updateCurrentHand currentPlayer.hands (\h -> { h | state = Standing })
                    }

                updatedPlayers =
                    ( updatedPlayer, players )

                allPlayersStandingOrBusted =
                    allPlayersHaveCond updatedPlayers (\h -> List.member h.state [ Standing, Busted ])
            in
            ( { model
                | players = ( updatedPlayer, Tuple.second model.players )
                , state =
                    if allPlayersStandingOrBusted then
                        DealerFinishes

                    else
                        model.state
              }
            , if allPlayersStandingOrBusted then
                Process.sleep 1000 |> Task.perform (\_ -> DealerFinish)

              else
                Cmd.none
            )

        -- Split current player hands into two, allow player to take a card/stand/split/doubledown on each hand
        Split ->
            let
                ( currentHand, rest ) =
                    currentPlayer.hands

                newHands : ( Hand, List Hand )
                newHands =
                    case currentHand.cards of
                        [ first, second ] ->
                            ( { cards = [ first ], bet = currentHand.bet, state = Playing, order = 0 }
                            , { cards = [ second ], bet = currentHand.bet, state = Playing, order = List.length rest + 1 } :: rest
                            )

                        _ ->
                            ( currentHand, rest )

                updatedPlayer =
                    { currentPlayer
                        | money = currentPlayer.money - currentHand.bet
                        , hands = newHands
                    }
            in
            ( { model | players = ( updatedPlayer, players ) }, Cmd.none )

        -- Double bet and take another card
        DoubleDown ->
            let
                currentBet =
                    Tuple.first currentPlayer.hands |> .bet

                ( cards, deck ) =
                    Deck.takeCard model.deck

                updatedPlayer =
                    { currentPlayer
                        | hands =
                            updateCurrentHand currentPlayer.hands
                                (\h ->
                                    { h
                                        | state = Standing
                                        , bet = h.bet * 2
                                        , cards = h.cards ++ cards
                                    }
                                )
                        , money = currentPlayer.money - (currentBet * 2)
                    }

                updatedPlayers =
                    ( updatedPlayer, players )

                allPlayersStandingOrBusted =
                    allPlayersHaveCond updatedPlayers (\h -> List.member h.state [ Standing, Busted ])
            in
            ( { model
                | players = updatedPlayers
                , deck = deck
                , state =
                    if allPlayersStandingOrBusted then
                        DealerFinishes

                    else
                        model.state
              }
            , if allPlayersStandingOrBusted then
                Process.sleep 1000 |> Task.perform (\_ -> DealerFinish)

              else
                Cmd.none
            )

        -- All players are now finished
        DealerFinish ->
            let
                lowestDealerHandValue =
                    model.dealer |> Hand.largestValue

                dealerHasReachedLimit =
                    lowestDealerHandValue >= 17

                dealerHasBust =
                    lowestDealerHandValue > 21

                updatedModel =
                    if dealerHasReachedLimit || dealerHasBust then
                        ( { model | state = Result }, Process.sleep 0 |> Task.perform (\_ -> Winnings) )

                    else
                        let
                            ( cards, deck ) =
                                Deck.takeCard model.deck
                        in
                        ( { model | dealer = model.dealer ++ cards, deck = deck }, Process.sleep 1000 |> Task.perform (\_ -> DealerFinish) )
            in
            updatedModel

        -- Dealer has busted or reached 17 now
        Winnings ->
            -- Calculate how many hands won/lost and add money to player
            let
                dealerHandValue =
                    Hand.largestValue model.dealer

                win =
                    calculateWinnings dealerHandValue currentPlayer

                totalBet =
                    currentPlayer.hands |> playerHands |> List.map .bet |> List.sum

                updatedPlayers =
                    ( { currentPlayer | money = currentPlayer.money + calculateWinnings dealerHandValue currentPlayer }
                    , List.map (\p -> { p | money = p.money + calculateWinnings dealerHandValue p }) players
                    )
            in
            ( { model | players = updatedPlayers }, Cmd.none )
                |> toast
                    (if win == 0 then
                        "You lost $" ++ String.fromInt totalBet ++ "!"

                     else
                        "You won $" ++ String.fromInt win ++ "!"
                    )

        NextRound ->
            let
                cleared =
                    ( clearHands currentPlayer, List.map clearHands (Tuple.second model.players) )

                newState =
                    initalState
            in
            ( { newState | state = Betting, deck = model.deck, players = cleared }, Cmd.none )

        ShowToast message ->
            ( { model | toast = Just message }, clearAlert )

        ClearToast ->
            ( { model | toast = Nothing }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        ( currentPlayer, _ ) =
            model.players
    in
    Html.div []
        ((if model.state /= Betting then
            dealerView model.dealer model.state
                :: List.map playerView (allPlayers model.players)

          else
            [ Html.text "" ]
         )
            ++ [ actionsView model.state currentPlayer
               , case model.toast of
                    Just message ->
                        toastView message

                    Nothing ->
                        Html.text ""
               ]
        )


cardView : Bool -> Card -> Html.Html msg
cardView hidden card =
    Html.div [ Html.Attributes.class "card" ]
        [ Html.div [ Html.Attributes.class "card-inner", Html.Attributes.classList [ ( "hidden", hidden ) ] ]
            [ Html.div [ Html.Attributes.class "color", Html.Attributes.class (Card.suiteToCssClass card) ] []
            , Html.div [ Html.Attributes.class "value", Html.Attributes.class (Card.valueToCssClass card) ] []
            ]
        ]


dealerView : Dealer -> GameState -> Html.Html Msg
dealerView dealer state =
    if List.member state [ DealerFinishes, Result ] then
        Html.div []
            [ Html.div [ Html.Attributes.class "cards" ] (List.map (cardView False) dealer)
            ]

    else
        case dealer of
            first :: [] ->
                Html.div []
                    [ Html.div [ Html.Attributes.class "cards" ]
                        [ cardView False first
                        ]
                    ]

            first :: second :: rest ->
                Html.div []
                    [ Html.div [ Html.Attributes.class "cards" ]
                        ([ cardView False first
                         , cardView (state /= Result) second
                         ]
                            ++ List.map (cardView False) rest
                        )
                    ]

            [] ->
                Html.p [] [ Html.text "Dealer has no cards yet" ]


playerView : Player -> Html.Html Msg
playerView player =
    let
        ( activeHand, otherHands ) =
            player.hands
    in
    Html.div [ Html.Attributes.class "hands" ]
        ((activeHand :: otherHands)
            |> List.sortBy .order
            |> List.map
                (\hand ->
                    if hand.order == activeHand.order then
                        activeHandView hand

                    else
                        inactiveHandView hand
                )
        )


inactiveHandView : Hand -> Html.Html msg
inactiveHandView =
    handView []


activeHandView : Hand -> Html.Html Msg
activeHandView ({ state } as hand) =
    handView [ Html.Attributes.classList [ ( "active", state == Playing ) ] ] hand


handView : List (Html.Attribute msg) -> Hand -> Html.Html msg
handView attributes { cards, state, bet } =
    Html.div ([ Html.Attributes.class "hand", Html.Attributes.classList [ ( "busted", state == Busted ) ] ] ++ attributes)
        [ Html.div []
            [ Html.div [ Html.Attributes.class "cards" ]
                (List.map (cardView False) cards)
            , Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "justify-content" "space-between" ]
                [ Html.p [] [ Html.text ("$" ++ String.fromInt bet) ]
                ]
            ]
        ]


actionsView : GameState -> Player -> Html.Html Msg
actionsView state player =
    Html.div [ Html.Attributes.class "actions" ]
        [ Html.p [] [ Html.text ("$" ++ String.fromInt player.money) ]
        , case state of
            Betting ->
                bettingView player

            ShuffleCards ->
                Html.text ""

            Dealing ->
                Html.text ""

            HitOrStand ->
                hitOrStandView player

            DealerFinishes ->
                Html.text ""

            Result ->
                Html.button [ Html.Events.onClick NextRound ] [ Html.text "Continue?" ]
        ]


bettingView : Player -> Html.Html Msg
bettingView { money } =
    Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
        (List.map
            (\amount ->
                Html.button [ Html.Events.onClick (Bet amount), Html.Attributes.disabled (money < amount) ] [ Html.text ("Bet $" ++ String.fromInt amount) ]
            )
            [ 1, 5, 10, 25, 50 ]
            ++ (if money > 50 then
                    [ Html.button [ Html.Events.onClick (Bet money) ] [ Html.text "All in!" ] ]

                else
                    []
               )
        )


hitOrStandView : Player -> Html.Html Msg
hitOrStandView { money, hands } =
    let
        ( { cards, bet, state }, _ ) =
            hands
    in
    Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
        [ Html.button [ Html.Events.onClick TakeCard, Html.Attributes.disabled (state /= Playing) ] [ Html.text "Take a card" ]
        , Html.button [ Html.Events.onClick Stand, Html.Attributes.disabled (state /= Playing) ] [ Html.text "Stand" ]
        , if canSplit cards then
            Html.button [ Html.Events.onClick Split, Html.Attributes.disabled (state /= Playing || money < bet) ] [ Html.text "Split" ]

          else
            Html.text ""
        , if List.length cards == 2 then
            Html.button
                [ Html.Events.onClick DoubleDown
                , Html.Attributes.disabled (bet * 2 > money)
                ]
                [ Html.text "Double down" ]

          else
            Html.text ""
        ]


toastView : String -> Html.Html msg
toastView message =
    Html.div [ Html.Attributes.class "toast" ]
        [ Html.div [ Html.Attributes.class "message " ] [ Html.text message ]
        ]



-- HELPER FUNCTIONS


allPlayers : ( Player, List Player ) -> List Player
allPlayers ( currentPlayer, rest ) =
    currentPlayer :: rest


allHands : List Player -> List Hand
allHands players =
    players
        |> List.map (\p -> playerHands p.hands)
        |> List.concat


allHandsHaveCond : Player -> (Hand -> Bool) -> Bool
allHandsHaveCond player cond =
    let
        ( currentHand, rest ) =
            player.hands
    in
    cond currentHand || List.all cond rest


playerHands : ( Hand, List Hand ) -> List Hand
playerHands ( currentHand, rest ) =
    currentHand :: rest


updateCurrentHand :
    ( Hand, List Hand )
    -> (Hand -> Hand)
    -> ( Hand, List Hand )
updateCurrentHand ( currentHand, rest ) new =
    ( new currentHand, rest )


clearHands : Player -> Player
clearHands player =
    { player | hands = ( { cards = [], bet = 0, state = Playing, order = 0 }, [] ) }


canSplit : Hand.Hand -> Bool
canSplit hand =
    case hand of
        [ first, second ] ->
            first.value == second.value

        _ ->
            False


continueToNextPlayer : Player -> List Player -> ( Player, List Player )
continueToNextPlayer currentPlayer rest =
    case rest of
        x :: xs ->
            ( x, xs ++ [ currentPlayer ] )

        _ ->
            ( currentPlayer, rest )


continueToNextHand : ( Hand, List Hand ) -> ( Hand, List Hand )
continueToNextHand ( currentHand, rest ) =
    case rest of
        x :: xs ->
            ( x, xs ++ [ currentHand ] )

        _ ->
            ( currentHand, rest )


calculateWinnings : Int -> Player -> Int
calculateWinnings dealerHand { hands } =
    List.foldr
        (\{ cards, bet, state } acc ->
            if state == Busted then
                -- You busted, no win
                acc

            else if dealerHand > 21 then
                -- Dealer busted, automatic win
                acc + bet * 2

            else
                case Basics.compare (Hand.largestValue cards) dealerHand of
                    GT ->
                        -- You won over the dealer, you get 2x your bet back
                        acc + bet * 2

                    EQ ->
                        -- Push, you get your inital bet back
                        acc + bet

                    LT ->
                        -- You lost to the dealer
                        acc
        )
        0
        (playerHands hands)


clearAlert : Cmd Msg
clearAlert =
    Process.sleep 2000 |> Task.perform (\_ -> ClearToast)


toast : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
toast message ( model, cmd ) =
    ( { model | toast = Just message }, Cmd.batch [ cmd, clearAlert ] )


toastIf : Bool -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
toastIf cond message =
    if cond then
        toast message

    else
        identity
