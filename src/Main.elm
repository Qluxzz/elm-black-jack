port module Main exposing
    ( Dealer
    , Dollars
    , Effect(..)
    , GameState
    , Model
    , Msg(..)
    , init
    , initWithDeck
    , main
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
import Platform.Cmd as Cmd
import Player
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
    = StartNewGame
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
    | ClearToast


type Effect
    = NoEffect
    | ShuffleDeck_ Deck.Deck
    | Deal_
    | DealerTakesCard_
    | TakeCard_
    | DealerFinish_
    | Winnings_
    | ClearToast_
    | UpdateHighScore Int
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

        UpdateHighScore highScore ->
            updateHighScore highScore

        Multiple effects ->
            Cmd.batch (List.map perform effects)



-- PORTS


port updateHighScore : Int -> Cmd msg



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
        ( { money = 500
          , hands = Player.emptyHands
          , order = 0
          }
        , []
        )
    , state = MainMenu
    , toast = Nothing
    , highScore = Nothing
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
    ( { initalState | highScore = flags.highScore }, NoEffect )


type alias Flags =
    { highScore : Maybe Int
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init >> Tuple.mapSecond perform
        , view =
            \model ->
                { title = "♦♣ Blackjack ♥♠"
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
        StartNewGame ->
            ( { initalState | highScore = model.highScore }, ShuffleDeck_ (Deck.decks 4) )

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
        -- Add card to player hand
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
                            ( { cards = [ first ], bet = currentHand.bet, state = Player.Playing, order = currentHand.order }
                              -- The new hand should always have the order after the current hand
                            , { cards = [ second ], bet = currentHand.bet, state = Player.Playing, order = currentHand.order + 1 }
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
                win =
                    Player.calculateWinnings model.dealer currentPlayer

                updatedPlayers =
                    ( { currentPlayer
                        | money =
                            currentPlayer.money + clamp 0 win win
                      }
                    , List.map
                        (\p ->
                            let
                                win_ =
                                    Player.calculateWinnings model.dealer p
                            in
                            { p | money = p.money + clamp 0 win_ win_ }
                        )
                        players
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
                |> withHighScore
                |> withToast
                    (Just
                        (if noMoney then
                            "You lost the game!"

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
            ( { newState | state = Betting, deck = model.deck, highScore = model.highScore, players = cleared }
            , if List.length model.deck < 15 then
                ShuffleDeck_ model.deck

              else
                NoEffect
            )

        ClearToast ->
            ( { model | toast = Nothing }, NoEffect )



-- VIEWS


view : Model -> List (Html.Html Msg)
view model =
    [ if model.state == MainMenu then
        mainMenuView model

      else
        let
            ( currentPlayer, _ ) =
                model.players
        in
        Html.div [ Html.Attributes.class "game" ]
            [ Html.div [ Html.Attributes.class "dealer-and-players" ]
                (if model.state /= Betting then
                    dealerView model.dealer model.state
                        :: List.map playerView (allPlayers model.players)

                 else
                    [ Html.text "" ]
                )
            , actionsView model.state currentPlayer
            ]
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
        [ Html.p [ Html.Attributes.class "player-money" ] [ Html.text ("Balance: $" ++ String.fromInt player.money) ]
        , case state of
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
                Html.button [ Html.Events.onClick NextRound ] [ Html.text "Continue?" ]
        ]


bettingView : Player.Player -> Html.Html Msg
bettingView { money } =
    let
        markerAmounts =
            [ 1, 10, 100, 500, 1000 ]

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


toastView : String -> Html.Html msg
toastView message =
    Html.div [ Html.Attributes.class "toast" ]
        [ Html.div [ Html.Attributes.class "message " ] [ Html.text message ]
        ]


mainMenuView : Model -> Html.Html Msg
mainMenuView model =
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
        , Html.p [] [ Html.text <| "Current high score: " ++ (model.highScore |> Maybe.map String.fromInt |> Maybe.map (\s -> "$" ++ s ++ "!") |> Maybe.withDefault "No high score yet!") ]
        , Html.button [ Html.Events.onClick StartNewGame ] [ Html.text "Start new game!" ]
        ]



-- HELPER FUNCTIONS


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


withHighScore : ( Model, Effect ) -> ( Model, Effect )
withHighScore ( model, effect ) =
    let
        playerMoney =
            model.players |> Tuple.first |> .money
    in
    case ( playerMoney, model.highScore ) of
        ( p, Just h ) ->
            if p > h then
                ( { model | highScore = Just p }, Multiple [ effect, UpdateHighScore p ] )

            else
                ( model, effect )

        ( p, Nothing ) ->
            if p > 0 then
                ( { model | highScore = Just p }, Multiple [ effect, UpdateHighScore p ] )

            else
                ( model, effect )
