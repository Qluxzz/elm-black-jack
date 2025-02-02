port module Main exposing
    ( Dealer
    , Dollars
    , Effect(..)
    , GameState
    , Model
    , Msg(..)
    , Statistics
    , defaultStatistics
    , init
    , initWithDeck
    , main
    , toDollars
    , update
    , view
    , withPlayer
    )

import Array
import Browser
import Card exposing (Card)
import Cards
import Deck
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Platform.Cmd as Cmd
import Player exposing (HandResult(..), HandState(..), Insurance(..))
import Process
import Random
import Random.List
import Task



{- Blackjack

   Game loop:

   All players in the game has to at least bet the minimum of the table
   The players are dealt two cards, the dealer is dealt two cards, but one is hidden

   Each player is given the choice to take another card or stand,
   When all players are standing or busted, the dealer flips the hidden card.

   If the dealers total sum is below the table minimum (often 17), the dealer takes cards until he reaches 17 or is bust.

   If the dealer busts, all players not busted win.
-}


type alias Statistics =
    { wins : Int
    , loses : Int
    , blackjack : Int
    , blackjackPush : Int
    , pushes : Int
    , doubleDowns : Int
    , roundsPlayed : Int
    , biggestHandWon : Dollars
    , biggestHandLost : Dollars
    , mostHandInSingleRound : Int
    , highestBalance : Dollars
    }


defaultStatistics : Statistics
defaultStatistics =
    { wins = 0
    , loses = 0
    , blackjack = 0
    , blackjackPush = 0
    , pushes = 0
    , doubleDowns = 0
    , roundsPlayed = 0
    , biggestHandWon = 0
    , biggestHandLost = 0
    , mostHandInSingleRound = 0
    , highestBalance = 0
    }


type alias Dollars =
    Int


type InsuranceAction
    = BuyInsurance
    | DeclineInsurance


type Msg
    = NoOp
    | StartNewGame
    | ShuffledDeck Deck.Deck
      -- Betting
    | Bet Dollars
      -- Deal
    | Deal
    | DealerTakesCard
      -- Hit or Stand
    | TakeActionOnInsurance InsuranceAction
    | TakeCard
    | Stand
    | Split
    | DoubleDown
    | NextRound
      -- All players are now standing or have busted
    | DealerFinish
    | Winnings
      -- Toast
    | ClearToast Id
    | ShowStatistics
    | HideStatistics
    | QuitToMainMenu


type Effect
    = NoEffect
    | ShuffleDeck_ Deck.Deck
    | Deal_
    | DealerTakesCard_
    | TakeCard_
    | DealerFinish_
    | Winnings_
    | ClearToast_ Id
    | UpdateStatistics Statistics
    | Multiple (List Effect)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        NoEffect ->
            Cmd.none

        ShuffleDeck_ deck ->
            Random.generate ShuffledDeck (Random.List.shuffle deck)

        Deal_ ->
            Process.sleep 0 |> Task.perform (\_ -> Deal)

        DealerTakesCard_ ->
            Process.sleep 1000 |> Task.perform (\_ -> DealerTakesCard)

        TakeCard_ ->
            Process.sleep 1000 |> Task.perform (\_ -> TakeCard)

        DealerFinish_ ->
            Process.sleep 1000 |> Task.perform (\_ -> DealerFinish)

        Winnings_ ->
            Process.sleep 1000 |> Task.perform (\_ -> Winnings)

        ClearToast_ id ->
            Process.sleep 2000 |> Task.perform (\_ -> ClearToast id)

        UpdateStatistics statistics ->
            updateStatistics statistics

        Multiple effects ->
            Cmd.batch (List.map perform effects)



-- PORTS


port updateStatistics : Statistics -> Cmd msg



-- END OF PORTS


type alias Dealer =
    List Card


type alias Id =
    Int


type alias Model =
    { state : GameState
    , deck : Deck.Deck
    , dealer : Dealer
    , player : Player.Player
    , toasts : List { id : Id, message : String }
    , highScore : Maybe Int
    , statistics : Statistics
    , showStatistics : Bool
    }


type GameState
    = MainMenu
    | Betting
    | Dealing
    | Insurance
    | HitOrStand
    | DealerFinishes
    | Result
    | ContinueToNextRound


initialState : Model
initialState =
    { deck = []
    , dealer = []
    , player =
        { money = 5000
        , hands = Player.emptyHands
        }
    , state = MainMenu
    , toasts = []
    , highScore = Nothing
    , statistics = defaultStatistics
    , showStatistics = False
    }


{-| Helper method to start with a deterministic deck
-}
initWithDeck : Deck.Deck -> Flags -> ( Model, Effect )
initWithDeck deck _ =
    ( { initialState | deck = deck, state = Betting }, NoEffect )


withPlayer : { money : Int } -> ( Model, Effect ) -> ( Model, Effect )
withPlayer { money } ( model, effect ) =
    ( { model | player = Player.Player Player.emptyHands money }, effect )


init : Flags -> ( Model, Effect )
init flags =
    ( { initialState
        | statistics = Maybe.withDefault defaultStatistics flags.statistics
      }
    , NoEffect
    )


type alias Flags =
    { statistics : Maybe Statistics
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init >> Tuple.mapSecond perform
        , view =
            \model ->
                { title = "♦♣\u{00A0}Blackjack\u{00A0}♥♠"
                , body = view model
                }
        , update = \msg model -> update msg model |> Tuple.mapSecond perform
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        NoOp ->
            ( model, NoEffect )

        StartNewGame ->
            ( { initialState | highScore = model.highScore, statistics = model.statistics }, ShuffleDeck_ (Deck.decks 4) )

        QuitToMainMenu ->
            ( { model | state = MainMenu }, NoEffect )

        ShuffledDeck deck ->
            ( { model | deck = deck, state = Betting }, NoEffect )

        {- Betting -}
        Bet amount ->
            let
                updatedPlayer =
                    model.player
                        |> Player.updatePlayer (\p -> { p | money = p.money - amount })
                        |> Player.updateCurrentHand (\h -> { h | bet = amount })

                allHandsHaveBet =
                    allHandsHaveCond (\h -> h.bet /= 0) updatedPlayer
            in
            ( { model
                | state =
                    if allHandsHaveBet then
                        Dealing

                    else
                        model.state
                , player = updatedPlayer
              }
            , if allHandsHaveBet then
                Deal_

              else
                NoEffect
            )

        {- DEALING -}
        Deal ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck

                ( updatedPlayer, toast ) =
                    model.player
                        |> Player.addCards cards
                        |> Player.addToastIfCurrentHandHas
                            (\h ->
                                if Cards.hasBlackjack h.cards then
                                    Just "Blackjack!"

                                else
                                    Nothing
                            )

                allHandsHaveTwoCards =
                    allHandsHaveCond (\h -> List.length h.cards == 2) updatedPlayer
            in
            ( { model
                | deck = deck
                , player = updatedPlayer
              }
            , if allHandsHaveTwoCards then
                DealerTakesCard_

              else if allHandsHaveCond (\h -> List.length h.cards == 1) updatedPlayer then
                DealerTakesCard_

              else if allHandsStandingOrBusted updatedPlayer then
                DealerFinish_

              else
                Deal_
            )
                |> withToast toast

        DealerTakesCard ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck

                updatedHand =
                    model.dealer ++ cards

                hasTwoCards =
                    List.length updatedHand == 2
            in
            ( { model
                | dealer = updatedHand
                , state =
                    if hasTwoCards then
                        if canBuyInsurance updatedHand model.player then
                            Insurance

                        else
                            HitOrStand

                    else
                        model.state
                , deck = deck
              }
            , if allHandsStandingOrBusted model.player then
                DealerFinish_

              else if hasTwoCards then
                NoEffect

              else
                Deal_
            )

        {- Hit or Stand -}
        TakeActionOnInsurance action ->
            let
                dealerHasBlackjack =
                    Cards.hasBlackjack model.dealer

                updatedPlayer =
                    case action of
                        DeclineInsurance ->
                            model.player

                        BuyInsurance ->
                            let
                                currentBet =
                                    model.player.hands |> Tuple.first |> .bet
                            in
                            model.player
                                |> Player.updateCurrentHand (\h -> { h | insurance = Insured (h.bet // 2), state = Playing })
                                |> Player.updatePlayer (\p -> { p | money = p.money - currentBet // 2 })
            in
            ( { model
                | player = updatedPlayer
                , state =
                    if dealerHasBlackjack then
                        DealerFinishes

                    else
                        HitOrStand
              }
            , if dealerHasBlackjack then
                Winnings_

              else
                NoEffect
            )
                |> withToast
                    (if not dealerHasBlackjack then
                        Just "Dealer didn't have blackjack!"

                     else
                        Nothing
                    )

        TakeCard ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck

                ( updatedPlayer, toast ) =
                    model.player
                        |> Player.addCards cards
                        |> Player.addToastIfCurrentHandHas
                            (\h ->
                                case ( Basics.compare (Cards.value h.cards) 21, List.length h.cards ) of
                                    ( GT, _ ) ->
                                        Just "Bust!"

                                    ( EQ, 2 ) ->
                                        Just "Blackjack!"

                                    _ ->
                                        Nothing
                            )
                        |> Tuple.mapFirst (Player.switchToNextHandIf (\h -> List.member h.state [ Player.Standing, Player.Busted ]))

                nextHandHasTwoCards =
                    updatedPlayer.hands |> Tuple.first |> (\h -> List.length h.cards >= 2)
            in
            ( { model
                | deck = deck
                , player = updatedPlayer
              }
            , if allHandsStandingOrBusted updatedPlayer then
                DealerFinish_

              else if not nextHandHasTwoCards then
                TakeCard_

              else
                NoEffect
            )
                |> withToast toast

        Stand ->
            let
                updatedPlayer =
                    model.player
                        |> Player.updateCurrentHand (\h -> { h | state = Player.Standing })
                        |> Player.switchToNextHand

                -- Can happen when splitting
                nextHandHasTwoCards =
                    updatedPlayer.hands
                        |> Tuple.first
                        |> (\h -> List.length h.cards >= 2)
            in
            ( { model | player = updatedPlayer }
            , if allHandsStandingOrBusted updatedPlayer then
                DealerFinish_

              else if not nextHandHasTwoCards then
                TakeCard_

              else
                NoEffect
            )

        -- Split current hand into two, allow player to take a card/stand/split/double down on each hand
        Split ->
            let
                ( currentHand, rest ) =
                    model.player.hands

                newHands : ( Player.Hand, List Player.Hand )
                newHands =
                    case currentHand.cards of
                        [ first, second ] ->
                            ( { currentHand | cards = [ first ] }
                              -- The new hand should always have the order after the current hand
                            , { currentHand | cards = [ second ], order = currentHand.order + 1 }
                                :: List.map
                                    (\h ->
                                        -- The order can loop around
                                        -- So as long as the hand's order is higher than the current hand's order we need to increase its order
                                        if h.order > currentHand.order then
                                            { h | order = h.order + 1 }

                                        else
                                            h
                                    )
                                    rest
                            )

                        _ ->
                            ( currentHand, rest )

                updatedPlayer =
                    model.player
                        |> (\p ->
                                { p
                                    | money = model.player.money - currentHand.bet
                                    , hands = newHands
                                }
                           )
            in
            ( { model | player = updatedPlayer }, TakeCard_ )

        -- Double bet and take another card
        DoubleDown ->
            let
                currentBet =
                    Tuple.first model.player.hands |> .bet

                ( cards, deck ) =
                    Deck.takeCard model.deck

                ( updatedPlayer, toast ) =
                    model.player
                        |> Player.addCards cards
                        |> Player.updatePlayer (\p -> { p | money = p.money - currentBet })
                        |> Player.updateCurrentHand
                            (\h ->
                                { h
                                    | bet = h.bet * 2
                                    , state =
                                        if h.state == Player.Playing then
                                            Player.Standing

                                        else
                                            h.state
                                }
                            )
                        |> Player.addToastIfCurrentHandHas
                            (\h ->
                                if Cards.value h.cards > 21 then
                                    Just "Bust!"

                                else
                                    Nothing
                            )
                        |> Tuple.mapFirst Player.switchToNextHand

                nextHandHasTwoCards =
                    updatedPlayer.hands
                        |> Tuple.first
                        |> (\h -> List.length h.cards >= 2)
            in
            ( { model
                | player = updatedPlayer
                , deck = deck
              }
            , if allHandsStandingOrBusted updatedPlayer then
                DealerFinish_

              else if not nextHandHasTwoCards then
                TakeCard_

              else
                NoEffect
            )
                |> withToast toast
                |> withDoubleDown

        -- All players are now finished
        DealerFinish ->
            let
                dealerHandValue =
                    model.dealer |> Cards.value

                dealerHasReachedLimit =
                    dealerHandValue >= 17

                dealerHasBust =
                    dealerHandValue > 21
            in
            if dealerHasReachedLimit || dealerHasBust then
                ( { model | state = Result }, Winnings_ )

            else
                ( { model | state = DealerFinishes }, DealerTakesCard_ )

        -- Dealer has busted or reached 17 now
        Winnings ->
            let
                handStates =
                    Player.calculateHandsState model.dealer model.player

                combined =
                    List.map2
                        Tuple.pair
                        handStates
                        (Player.playerHands model.player.hands)

                roundResult : RoundResult
                roundResult =
                    List.foldr
                        (\( s, { bet } ) acc ->
                            let
                                w =
                                    Player.calculateWinnings bet s
                            in
                            case s of
                                Lost ->
                                    { acc | lost = acc.lost + 1, biggestHandLost = max acc.biggestHandLost (abs w) }

                                Blackjack ->
                                    { acc | blackjack = acc.blackjack + 1, biggestHandWon = max acc.biggestHandWon w }

                                Won ->
                                    { acc | won = acc.won + 1, biggestHandWon = max acc.biggestHandWon w }

                                Push ->
                                    { acc | push = acc.push + 1, biggestHandWon = max acc.biggestHandWon w }

                                BlackjackPush ->
                                    { acc | blackjackPush = acc.blackjackPush + 1, biggestHandWon = max acc.biggestHandWon win }
                        )
                        { lost = 0, won = 0, blackjack = 0, blackjackPush = 0, push = 0, biggestHandWon = 0, biggestHandLost = 0, handsInRound = List.length combined }
                        combined

                win =
                    let
                        dealerHasBlackjack =
                            Cards.hasBlackjack model.dealer
                    in
                    List.foldr
                        (\( state, { bet, insurance } ) acc ->
                            case ( dealerHasBlackjack, insurance ) of
                                -- Insurance only pays out if the dealer had a blackjack
                                ( True, Insured amount ) ->
                                    acc + Player.calculateWinnings bet state + (amount * 2)

                                _ ->
                                    acc + Player.calculateWinnings bet state
                        )
                        0
                        combined

                updatedPlayer =
                    model.player
                        |> (\p -> { p | money = p.money + clamp 0 win win })

                noMoney =
                    updatedPlayer.money == 0
            in
            ( { model
                | player = updatedPlayer
                , state =
                    if noMoney then
                        MainMenu

                    else
                        ContinueToNextRound
              }
            , NoEffect
            )
                |> withRoundResult roundResult
                |> withToast
                    (Just
                        (if noMoney then
                            "You lost the game!"

                         else if win == 0 then
                            "You got your bet back!"

                         else if win < 0 then
                            "You lost $" ++ String.fromInt (abs win) ++ "!"

                         else
                            "You won $" ++ String.fromInt win ++ "!"
                        )
                    )

        NextRound ->
            ( { initialState
                | state = Betting
                , deck = model.deck
                , highScore = model.highScore
                , statistics = model.statistics
                , player = Player.clearHands model.player
              }
            , if List.length model.deck < 20 then
                ShuffleDeck_ (Deck.decks 4)

              else
                NoEffect
            )

        ClearToast id ->
            ( { model | toasts = List.filter (\t -> t.id /= id) model.toasts }, NoEffect )

        ShowStatistics ->
            ( { model | showStatistics = True }, NoEffect )

        HideStatistics ->
            ( { model | showStatistics = False }, NoEffect )



-- VIEWS


view : Model -> List (Html.Html Msg)
view model =
    (if model.state == MainMenu then
        [ mainMenuView
        , if model.showStatistics then
            statisticsView model.statistics

          else
            Html.text ""
        ]

     else
        [ Html.div [ Html.Attributes.class "game" ]
            [ Html.header [] [ Html.p [ Html.Attributes.class "player-money" ] [ Html.text ("Balance: $" ++ String.fromInt model.player.money) ] ]
            , Html.div
                [ Html.Attributes.class "dealer-and-players" ]
                (if model.state /= Betting then
                    [ dealerView model.dealer model.state, playerView model.player ]

                 else
                    [ Html.text "" ]
                )
            , actionsView model.state model.player
            ]
        ]
    )
        ++ [ toastView model.toasts ]


cardColorAndSuite : Card -> List (Html.Html msg)
cardColorAndSuite card =
    [ Html.div [ Html.Attributes.class "color", Html.Attributes.class (Card.suiteToCssClass card) ] []
    , Html.div [ Html.Attributes.class "value", Html.Attributes.class (Card.valueToCssClass card) ] []
    ]


hiddenCard : Bool -> Card -> Html.Html msg
hiddenCard hidden card =
    cardView_
        [ Html.div [ Html.Attributes.class "card-flip", Html.Attributes.classList [ ( "hidden", hidden ) ] ]
            [ Html.div [ Html.Attributes.class "card-front" ]
                (cardColorAndSuite card)
            , Html.div [ Html.Attributes.class "card-back" ] []
            ]
        ]
        card


cardView_ : List (Html.Html msg) -> Card -> Html.Html msg
cardView_ children card =
    Html.div [ Html.Attributes.class "card", Html.Attributes.attribute "test-id" (Card.toString card) ]
        [ Html.div [ Html.Attributes.class "card-inner" ]
            children
        ]


cardView : Card -> Html.Html msg
cardView card =
    cardView_ (cardColorAndSuite card) card


dealerView : Dealer -> GameState -> Html.Html Msg
dealerView dealer state =
    let
        hideSecondCard =
            not <| List.member state [ DealerFinishes, Result, ContinueToNextRound ]
    in
    Html.div [ Html.Attributes.class "dealer", Html.Attributes.class "hand", Html.Attributes.classList [ ( "busted", Cards.value dealer > 21 ) ] ]
        [ Html.div [ Html.Attributes.class "cards" ]
            (List.indexedMap
                (\i -> hiddenCard (i == 1 && hideSecondCard))
                dealer
            )
        , if List.length dealer > 0 then
            Html.p []
                [ Html.text
                    (String.fromInt
                        (Cards.value
                            (if hideSecondCard then
                                List.take 1 dealer

                             else
                                dealer
                            )
                        )
                    )
                ]

          else
            Html.text ""
        ]


playerView : Player.Player -> Html.Html Msg
playerView player =
    let
        ( activeHand, otherHands ) =
            player.hands
    in
    Html.div [ Html.Attributes.class "player" ]
        [ Html.div [ Html.Attributes.class "hands" ]
            ((activeHand :: otherHands)
                |> List.sortBy .order
                |> List.map
                    (\({ state } as hand) ->
                        handView
                            [ Html.Attributes.classList
                                [ ( "active", state == Player.Playing && hand.order == activeHand.order )
                                , ( "busted", state == Player.Busted )
                                ]
                            ]
                            hand
                    )
            )
        ]


handView : List (Html.Attribute msg) -> Player.Hand -> Html.Html msg
handView attributes { cards, bet } =
    Html.div (Html.Attributes.class "hand" :: attributes)
        [ Html.div [ Html.Attributes.class "cards" ]
            (List.map cardView cards)
        , Html.div [ Html.Attributes.class "stats" ]
            [ Html.p [] [ Html.text (String.fromInt (Cards.value cards)) ]
            , Html.p [] [ Html.text ("$" ++ String.fromInt bet) ]
            ]
        ]


actionsView : GameState -> Player.Player -> Html.Html Msg
actionsView state player =
    Html.div [ Html.Attributes.class "actions" ]
        [ case state of
            MainMenu ->
                Html.text ""

            Betting ->
                bettingView player

            Dealing ->
                Html.text ""

            Insurance ->
                insuranceView

            HitOrStand ->
                hitOrStandView player

            DealerFinishes ->
                Html.text ""

            Result ->
                Html.text ""

            ContinueToNextRound ->
                Html.div [ Html.Attributes.class "end-of-round" ]
                    [ Html.button [ Html.Events.onClick NextRound ]
                        [ Html.text "Continue?" ]

                    -- Hidden quit button used for tests
                    , Html.button
                        [ Html.Events.onClick QuitToMainMenu, Html.Attributes.style "display" "none" ]
                        [ Html.text "Quit?" ]
                    ]
        ]


bettingView : Player.Player -> Html.Html Msg
bettingView { money } =
    let
        markerAmounts =
            [ 50, 100, 500, 1000 ]

        showAllInButton =
            List.maximum markerAmounts
                |> Maybe.map (\v -> money > v)
                |> Maybe.withDefault False
    in
    Html.div
        [ Html.Attributes.class "betting" ]
        ((markerAmounts
            |> List.map
                (\amount ->
                    Html.button
                        [ Html.Attributes.class "marker"
                        , Html.Attributes.class ("_" ++ String.fromInt amount)
                        , Html.Attributes.disabled (money < amount)
                        , Html.Events.onClick (Bet amount)
                        ]
                        [ Html.text ("$" ++ String.fromInt amount) ]
                )
         )
            ++ (if showAllInButton then
                    [ Html.button [ Html.Events.onClick (Bet money) ] [ Html.text "All in!" ] ]

                else
                    []
               )
        )


hitOrStandView : Player.Player -> Html.Html Msg
hitOrStandView { money, hands } =
    let
        ( { cards, bet, state }, _ ) =
            hands

        allDisabled =
            List.length cards == 1 || state /= Player.Playing
    in
    Html.div [ Html.Attributes.class "hit-or-stand" ]
        [ Html.button [ Html.Events.onClick TakeCard, Html.Attributes.disabled allDisabled ] [ Html.text "Hit" ]
        , Html.button [ Html.Events.onClick Stand, Html.Attributes.disabled allDisabled ] [ Html.text "Stand" ]
        , if Cards.canSplit cards then
            Html.button [ Html.Events.onClick Split, Html.Attributes.disabled (allDisabled || money < bet) ] [ Html.text "Split" ]

          else
            Html.text ""
        , if List.length cards == 2 then
            Html.button
                [ Html.Events.onClick DoubleDown
                , Html.Attributes.disabled (money < bet || allDisabled)
                ]
                [ Html.text "Double down" ]

          else
            Html.text ""
        ]


toastView : List { id : Int, message : String } -> Html.Html msg
toastView toasts =
    Html.div [ Html.Attributes.class "overlay" ] (List.map (\{ message, id } -> Html.div [ Html.Attributes.class "message", Html.Attributes.id <| ("toast-" ++ String.fromInt id) ] [ Html.h1 [] [ Html.text message ] ]) toasts)


insuranceView : Html.Html Msg
insuranceView =
    Html.div [ Html.Attributes.class "overlay" ]
        [ Html.div [ Html.Attributes.class "message" ]
            [ Html.h1 [] [ Html.text "Buy insurance?" ]
            , Html.p [] [ Html.text "Costs 50% of your bet, if dealer has blackjack, you win your initial bet back" ]
            , Html.div [ Html.Attributes.class "button-group" ]
                [ Html.button [ Html.Events.onClick (TakeActionOnInsurance BuyInsurance) ] [ Html.text "Yes" ]
                , Html.button [ Html.Events.onClick (TakeActionOnInsurance DeclineInsurance) ] [ Html.text "No" ]
                ]
            ]
        ]


mainMenuView : Html.Html Msg
mainMenuView =
    let
        suites =
            Array.fromList [ Card.Clubs, Card.Diamonds, Card.Spades, Card.Hearts ]

        logoCard : Char -> Html.Html msg
        logoCard char =
            Html.div [ Html.Attributes.class "card" ]
                [ Html.div [ Html.Attributes.class "card-inner" ]
                    [ Html.div [ Html.Attributes.class "character" ] [ Html.text (String.fromChar char) ]
                    , Html.div
                        [ Html.Attributes.class "color"
                        , Html.Attributes.class
                            (Card.suiteToCssClass
                                (char
                                    |> Char.toCode
                                    |> Basics.modBy (Array.length suites)
                                    |> (\i ->
                                            Array.get i suites
                                                |> Maybe.map (\s -> Card.Card Card.Ace s)
                                                |> Maybe.withDefault (Card.Card Card.Ace Card.Spades)
                                       )
                                )
                            )
                        ]
                        []
                    ]
                ]
    in
    Html.div [ Html.Attributes.class "main-menu" ]
        [ Html.div [ Html.Attributes.class "logo" ] (List.map logoCard (String.toList "Blackjack!"))
        , Html.div [ Html.Attributes.class "menu-items" ]
            [ Html.button [ Html.Events.onClick StartNewGame ] [ Html.text "Start new game!" ]
            , Html.button [ Html.Events.onClick ShowStatistics ] [ Html.text "Statistics" ]
            ]
        ]


statisticsView : Statistics -> Html.Html Msg
statisticsView statistics =
    let
        list : List { title : String, description : Maybe String, value : String }
        list =
            [ { title = "Rounds played", description = Nothing, value = String.fromInt statistics.roundsPlayed }
            , { title = "Hands won", description = Nothing, value = String.fromInt statistics.wins }
            , { title = "Hands lost", description = Nothing, value = String.fromInt statistics.loses }
            , { title = "Win rate", description = Nothing, value = formatWinRate (toFloat statistics.wins / (toFloat <| Basics.clamp 1 (statistics.wins + statistics.loses) (statistics.wins + statistics.loses)) * 100) ++ "%" }
            , { title = "Blackjack", description = Nothing, value = String.fromInt statistics.blackjack }
            , { title = "Blackjack push", description = Just "You and the dealer both got blackjack", value = String.fromInt statistics.blackjackPush }
            , { title = "Push", description = Nothing, value = String.fromInt statistics.pushes }
            , { title = "Double down", description = Nothing, value = String.fromInt statistics.doubleDowns }
            , { title = "Biggest hand won", description = Nothing, value = toDollars statistics.biggestHandWon }
            , { title = "Biggest hand lost", description = Nothing, value = toDollars statistics.biggestHandLost }
            , { title = "Most hands in a round", description = Just "How many hands you got by splitting in a single round", value = String.fromInt statistics.mostHandInSingleRound }
            , { title = "Highest balance", description = Nothing, value = toDollars statistics.highestBalance }
            ]
    in
    Html.div [ Html.Attributes.class "statistics", Html.Events.onClick HideStatistics ]
        [ Html.div [ Html.Attributes.class "inner", Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( NoOp, True )) ]
            [ Html.dl []
                (List.map
                    (\{ title, description, value } ->
                        case description of
                            Just d ->
                                [ Html.dt []
                                    [ Html.text (title ++ ": " ++ value)
                                    ]
                                , Html.dd [] [ Html.text d ]
                                ]

                            Nothing ->
                                [ Html.dt [] [ Html.text (title ++ ": " ++ value) ] ]
                    )
                    list
                    |> List.concat
                )
            ]
        ]



-- HELPER FUNCTIONS


{-| If number has decimals, round to two decimals, otherwise just return integer part
-}
formatWinRate : Float -> String
formatWinRate value =
    if toFloat (round value) == value then
        String.fromFloat value

    else
        let
            factor =
                10.0 ^ 2

            rounded =
                round (value * factor) |> toFloat

            adjusted =
                rounded / factor

            baseString =
                String.fromFloat adjusted

            parts =
                String.split "." baseString
        in
        case parts of
            [ integerPart, decimalPart ] ->
                let
                    paddedDecimals =
                        String.padRight 2 '0' decimalPart
                in
                integerPart ++ "." ++ paddedDecimals

            _ ->
                -- Fallback for unexpected cases
                ""


allHands : Player.Player -> List Player.Hand
allHands =
    .hands >> Player.playerHands


allHandsHaveCond : (Player.Hand -> Bool) -> Player.Player -> Bool
allHandsHaveCond cond =
    allHands >> List.all cond


allHandsStandingOrBusted : Player.Player -> Bool
allHandsStandingOrBusted =
    allHandsHaveCond (\h -> List.member h.state [ Player.Standing, Player.Busted ])


withToast : Maybe String -> ( Model, Effect ) -> ( Model, Effect )
withToast message ( model, effect ) =
    case message of
        Just m ->
            let
                id =
                    List.length model.toasts
            in
            ( { model | toasts = { id = id, message = m } :: model.toasts }, Multiple [ effect, ClearToast_ id ] )

        Nothing ->
            ( model, effect )


withDoubleDown : ( Model, Effect ) -> ( Model, Effect )
withDoubleDown ( model, effect ) =
    let
        updatedStatistics =
            model.statistics
                |> (\s -> { s | doubleDowns = s.doubleDowns + 1 })
    in
    ( { model | statistics = updatedStatistics }, Multiple [ effect, UpdateStatistics updatedStatistics ] )


type alias RoundResult =
    { lost : Int
    , won : Int
    , blackjack : Int
    , blackjackPush : Int
    , push : Int
    , biggestHandWon : Dollars
    , biggestHandLost : Dollars
    , handsInRound : Int
    }


withRoundResult : RoundResult -> ( Model, Effect ) -> ( Model, Effect )
withRoundResult { lost, won, blackjack, blackjackPush, push, biggestHandLost, biggestHandWon, handsInRound } ( model, effect ) =
    let
        updatedStatistics =
            model.statistics
                |> (\s ->
                        { s
                            | wins = s.wins + won + blackjack
                            , loses = s.loses + lost
                            , pushes = s.pushes + push
                            , blackjack = s.blackjack + blackjack + blackjackPush
                            , blackjackPush = s.blackjackPush + blackjackPush
                            , roundsPlayed = s.roundsPlayed + 1
                            , biggestHandWon = max s.biggestHandWon biggestHandWon
                            , biggestHandLost = max s.biggestHandLost biggestHandLost
                            , mostHandInSingleRound = max s.mostHandInSingleRound handsInRound
                            , highestBalance = max s.highestBalance model.player.money
                        }
                   )
    in
    ( { model | statistics = updatedStatistics }, Multiple [ effect, UpdateStatistics updatedStatistics ] )


toDollars : Int -> String
toDollars amount =
    "$" ++ String.fromInt amount


canBuyInsurance : List Card.Card -> Player.Player -> Bool
canBuyInsurance cards player =
    case cards of
        first :: _ ->
            let
                playerHasBlackJack =
                    Cards.hasBlackjack (player.hands |> Tuple.first |> .cards)

                playerHasEnoughMoney =
                    player.money > (player.hands |> Tuple.first |> .bet) // 2
            in
            (first.value == Card.Ace)
                && not playerHasBlackJack
                && playerHasEnoughMoney

        _ ->
            False
