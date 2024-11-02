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
    , withPlayers
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



{- Black Jack

   Game loop:

   All players in the game has to at least bet the minimum of the table
   The players are dealed two cards, the dealer is dealed two cards, but one is hidden

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
    | BuyInsurance
    | DeclineInsurance
    | TakeCard
    | Stand
    | Split
    | DoubleDown
    | NextRound
      -- All players are now standing or have busted
    | DealerFinish
    | Winnings
      -- Toast
    | ClearToast
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
    | ClearToast_
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

        ClearToast_ ->
            Process.sleep 2000 |> Task.perform (\_ -> ClearToast)

        UpdateStatistics statistics ->
            updateStatistics statistics

        Multiple effects ->
            Cmd.batch (List.map perform effects)



-- PORTS


port updateStatistics : Statistics -> Cmd msg



-- END OF PORTS


type alias Dealer =
    List Card


type alias Model =
    { state : GameState
    , deck : Deck.Deck
    , dealer : Dealer
    , players : ( Player.Player, List Player.Player )
    , toast : Maybe String
    , highScore : Maybe Int
    , statistics : Statistics
    , showStatistics : Bool
    }


type GameState
    = MainMenu
    | ShuffleCards -- Only done at start and if not enough cards left for a new round, TODO: Implement cutting the deck?
    | Betting
    | Dealing
    | HitOrStand
    | DealerFinishes
    | Result
    | ContinueToNextRound


initalState : Model
initalState =
    { deck = []
    , dealer = []
    , players =
        ( { money = 5000
          , hands = Player.emptyHands
          , order = 0
          }
        , []
        )
    , state = MainMenu
    , toast = Nothing
    , highScore = Nothing
    , statistics = defaultStatistics
    , showStatistics = False
    }


{-| Helper method to start with a deterministic deck
-}
initWithDeck : Deck.Deck -> ( Model, Effect )
initWithDeck deck =
    ( { initalState | deck = deck, state = Betting }, NoEffect )


withPlayers : ( { money : Int }, List { money : Int } ) -> ( Model, Effect ) -> ( Model, Effect )
withPlayers ( f, r ) ( model, effect ) =
    ( { model
        | players =
            case List.indexedMap (\i { money } -> Player.Player Player.emptyHands money i) (f :: r) of
                first :: rest ->
                    ( first, rest )

                _ ->
                    ( Player.Player Player.emptyHands f.money 0, [] )
      }
    , effect
    )


init : Flags -> ( Model, Effect )
init flags =
    ( { initalState
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
        { init =
            \_ ->
                initWithDeck
                    [ Card.Card Card.Ten Card.Spades
                    , Card.Card Card.Ace Card.Spades
                    , Card.Card Card.King Card.Diamonds
                    , Card.Card Card.King Card.Clubs
                    ]
                    |> Tuple.mapSecond perform
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
    let
        ( currentPlayer, players ) =
            model.players
    in
    case msg of
        NoOp ->
            ( model, NoEffect )

        StartNewGame ->
            ( { initalState | highScore = model.highScore, statistics = model.statistics }, ShuffleDeck_ (Deck.decks 4) )

        QuitToMainMenu ->
            ( { model | state = MainMenu }, NoEffect )

        ShuffledDeck deck ->
            ( { model | deck = deck, state = Betting }, NoEffect )

        {- Betting -}
        Bet amount ->
            let
                updatedPlayer =
                    currentPlayer
                        |> Player.updatePlayer (\p -> { p | money = p.money - amount })
                        |> Player.updateCurrentHand (\h -> { h | bet = amount })

                allHandsHaveBet =
                    allPlayerHandsHaveCond (\h -> h.bet /= 0) ( updatedPlayer, players )
            in
            ( { model
                | state =
                    if allHandsHaveBet then
                        Dealing

                    else
                        model.state
                , players = Player.switchToNextPlayerIf allHandsHaveBet ( updatedPlayer, players )
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
                    currentPlayer
                        |> Player.addCards cards
                        |> Player.addToastIfCurrentHandHas
                            (\h ->
                                if Cards.hasBlackJack h.cards then
                                    Just "Black Jack!"

                                else
                                    Nothing
                            )

                currentHandHasTwoCards =
                    currentPlayer.hands
                        |> Tuple.first
                        |> (\selectedHand ->
                                List.length selectedHand.cards == 2
                           )

                updatedPlayers =
                    Player.switchToNextPlayerIf currentHandHasTwoCards ( updatedPlayer, players )

                allHandsHaveTwoCards =
                    allPlayerHandsHaveCond (\h -> List.length h.cards == 2) updatedPlayers
            in
            ( { model
                | deck = deck
                , players = updatedPlayers
              }
            , if allHandsHaveTwoCards then
                DealerTakesCard_

              else if allPlayerHandsHaveCond (\h -> List.length h.cards == 1) updatedPlayers then
                DealerTakesCard_

              else if allPlayersStandingOrBusted updatedPlayers then
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
                        HitOrStand

                    else
                        model.state
                , deck = deck
              }
            , if allPlayersStandingOrBusted model.players then
                DealerFinish_

              else if hasTwoCards then
                NoEffect

              else
                Deal_
            )

        {- Hit or Stand -}
        BuyInsurance ->
            let
                currentBet =
                    currentPlayer |> .hands |> Tuple.first |> .bet

                updatedPlayer =
                    currentPlayer
                        |> Player.updateCurrentHand (\h -> { h | insurance = Insured (h.bet // 2), state = Standing })
                        |> Player.updatePlayer (\p -> { p | money = p.money - currentBet // 2 })

                updatedPlayers =
                    ( updatedPlayer, players )

                allallPlayersStandingOrBusted_ =
                    allPlayersStandingOrBusted updatedPlayers
            in
            ( { model | players = updatedPlayers }
            , if allallPlayersStandingOrBusted_ then
                DealerFinish_

              else
                NoEffect
            )

        DeclineInsurance ->
            ( model, NoEffect )

        TakeCard ->
            let
                ( cards, deck ) =
                    Deck.takeCard model.deck

                ( updatedPlayer, toast ) =
                    currentPlayer
                        |> Player.addCards cards
                        |> Player.addToastIfCurrentHandHas
                            (\h ->
                                case ( Basics.compare (Cards.value h.cards) 21, List.length h.cards ) of
                                    ( GT, _ ) ->
                                        Just "Bust!"

                                    ( EQ, 2 ) ->
                                        Just "Black Jack!"

                                    _ ->
                                        Nothing
                            )
                        |> Tuple.mapFirst (Player.switchToNextHandIf (\h -> List.member h.state [ Player.Standing, Player.Busted ]))

                updatedPlayers =
                    ( updatedPlayer, players )

                nextHandHasTwoCards =
                    updatedPlayer.hands |> Tuple.first |> (\h -> List.length h.cards >= 2)
            in
            ( { model
                | deck = deck
                , players = updatedPlayers
              }
            , if allPlayersStandingOrBusted updatedPlayers then
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
                    currentPlayer
                        |> Player.updateCurrentHand (\h -> { h | state = Player.Standing })
                        |> Player.switchToNextHand

                -- Can happen when splitting
                nextHandHasTwoCards =
                    updatedPlayer.hands
                        |> Tuple.first
                        |> (\h -> List.length h.cards >= 2)

                updatedPlayers =
                    ( updatedPlayer, players )
            in
            ( { model | players = updatedPlayers }
            , if allPlayersStandingOrBusted updatedPlayers then
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
                    currentPlayer.hands

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
                    { currentPlayer
                        | money = currentPlayer.money - currentHand.bet
                        , hands = newHands
                    }
            in
            ( { model | players = ( updatedPlayer, players ) }, TakeCard_ )

        -- Double bet and take another card
        DoubleDown ->
            let
                currentBet =
                    Tuple.first currentPlayer.hands |> .bet

                ( cards, deck ) =
                    Deck.takeCard model.deck

                ( updatedPlayer, toast ) =
                    currentPlayer
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

                updatedPlayers =
                    ( updatedPlayer, players )

                allPlayersStandingOrBusted_ =
                    allPlayersStandingOrBusted updatedPlayers

                nextHandHasTwoCards =
                    updatedPlayer.hands
                        |> Tuple.first
                        |> (\h -> List.length h.cards >= 2)
            in
            ( { model
                | players = updatedPlayers
                , deck = deck
              }
            , if allPlayersStandingOrBusted_ then
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
                    Player.calculateHandsState model.dealer currentPlayer

                combined =
                    List.map2
                        Tuple.pair
                        handStates
                        (Player.playerHands currentPlayer.hands)

                roundResult : RoundResult
                roundResult =
                    List.foldr
                        (\( s, hand ) acc ->
                            let
                                w =
                                    Player.calculateWinnings hand s
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
                    List.foldr
                        (\( state, hand ) acc ->
                            acc + Player.calculateWinnings hand state
                        )
                        0
                        combined

                updatedPlayers =
                    ( { currentPlayer
                        | money =
                            currentPlayer.money + clamp 0 win win
                      }
                    , players
                    )

                noMoney =
                    updatedPlayers |> Tuple.first |> .money |> (==) 0
            in
            ( { model
                | players = updatedPlayers
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
            let
                cleared =
                    ( Player.clearHands currentPlayer, List.map Player.clearHands (Tuple.second model.players) )

                newState =
                    initalState
            in
            ( { newState | state = Betting, deck = model.deck, highScore = model.highScore, statistics = model.statistics, players = cleared }
            , if List.length model.deck < 20 then
                ShuffleDeck_ (Deck.decks 4)

              else
                NoEffect
            )

        ClearToast ->
            ( { model | toast = Nothing }, NoEffect )

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
        let
            ( currentPlayer, _ ) =
                model.players
        in
        [ Html.div [ Html.Attributes.class "game" ]
            [ Html.header [] [ Html.p [ Html.Attributes.class "player-money" ] [ Html.text ("Balance: $" ++ String.fromInt currentPlayer.money) ] ]
            , Html.div
                [ Html.Attributes.class "dealer-and-players" ]
                (if model.state /= Betting then
                    dealerView model.dealer model.state
                        :: List.map playerView (allPlayers model.players)

                 else
                    [ Html.text "" ]
                )
            , actionsView model.state currentPlayer
            ]
        ]
    )
        ++ [ if model.state == HitOrStand && canBuyInsurance model.dealer then
                insuranceView

             else
                Html.text ""
           , Maybe.map toastView model.toast |> Maybe.withDefault (Html.text "")
           ]


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

            ShuffleCards ->
                Html.text ""

            Dealing ->
                Html.text ""

            HitOrStand ->
                hitOrStandView player

            DealerFinishes ->
                Html.text ""

            Result ->
                Html.text ""

            ContinueToNextRound ->
                Html.div []
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
    -- TODO: If dealer has an ace, offer player to buy insurance, costs 50% of the original bet, payout is 2 to 1 if dealer has blackjack
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


toastView : String -> Html.Html msg
toastView message =
    Html.div [ Html.Attributes.class "overlay" ]
        [ Html.div [ Html.Attributes.class "message" ] [ Html.text message ]
        ]


insuranceView : Html.Html Msg
insuranceView =
    Html.div [ Html.Attributes.class "overlay" ]
        [ Html.div [ Html.Attributes.class "message" ]
            [ Html.h1 [] [ Html.text "Buy insurance?" ]
            , Html.div [ Html.Attributes.class "button-group" ]
                [ Html.button [ Html.Events.onClick BuyInsurance ] [ Html.text "Yes" ]
                , Html.button [ Html.Events.onClick DeclineInsurance ] [ Html.text "No" ]
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


allPlayers : ( Player.Player, List Player.Player ) -> List Player.Player
allPlayers ( currentPlayer, rest ) =
    currentPlayer :: rest


allHands : List Player.Player -> List Player.Hand
allHands =
    List.concatMap (.hands >> Player.playerHands)


allPlayerHandsHaveCond : (Player.Hand -> Bool) -> ( Player.Player, List Player.Player ) -> Bool
allPlayerHandsHaveCond cond =
    allPlayers >> allHands >> List.all cond


allPlayersStandingOrBusted : ( Player.Player, List Player.Player ) -> Bool
allPlayersStandingOrBusted =
    allPlayerHandsHaveCond (\h -> List.member h.state [ Player.Standing, Player.Busted ])


withToast : Maybe String -> ( Model, Effect ) -> ( Model, Effect )
withToast message ( model, effect ) =
    case message of
        Just m ->
            ( { model | toast = Just m }, Multiple [ effect, ClearToast_ ] )

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
                            , highestBalance = max s.highestBalance (model.players |> Tuple.first |> .money)
                        }
                   )
    in
    ( { model | statistics = updatedStatistics }, Multiple [ effect, UpdateStatistics updatedStatistics ] )


toDollars : Int -> String
toDollars amount =
    "$" ++ String.fromInt amount


canBuyInsurance : List Card.Card -> Bool
canBuyInsurance cards =
    case cards of
        first :: _ ->
            first.value == Card.Ace

        _ ->
            False
