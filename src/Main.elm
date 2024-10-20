module Main exposing (Effect(..), Model, Msg(..), Player, emptyHands, init, initWithDeck, main, update, view)

import Browser
import Card exposing (Card)
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
    = ShuffledDeck Deck.Deck
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
    | NoOp


type Effect
    = NoEffect
    | ShuffleDeck_ Deck.Deck
      -- Betting
    | Deal_
    | DealerTakesCard_
    | TakeCard_
    | DealerFinish_
    | Winnings_
      -- Toast
    | ClearToast_
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
            Process.sleep 0 |> Task.perform (\_ -> DealerFinish)

        Winnings_ ->
            Process.sleep 0 |> Task.perform (\_ -> Winnings)

        ClearToast_ ->
            Process.sleep 2000 |> Task.perform (\_ -> ClearToast)

        Multiple effects ->
            Cmd.batch (List.map perform effects)


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
    , order : Int
    }


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
        ( { money = 500
          , hands = emptyHands
          , order = 0
          }
        , []
        )
    , state = ShuffleCards
    , toast = Nothing
    }


{-| Helper method to start with a deterministic deck
-}
initWithDeck : Deck.Deck -> ( Model, Effect )
initWithDeck deck =
    ( { initalState | deck = deck, state = Betting }, NoEffect )


init : ( Model, Effect )
init =
    ( initalState, ShuffleDeck_ (Deck.decks 4) )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init |> Tuple.mapSecond perform
        , view = \model -> view model
        , update = \msg model -> update msg model |> Tuple.mapSecond perform
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Model -> ( Model, Effect )
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
        NoOp ->
            ( model, NoEffect )

        ShuffledDeck deck ->
            ( { model | deck = deck, state = Betting }, NoEffect )

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
                        nextPlayer updatedPlayer players

                    else
                        ( updatedPlayer, players )
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
                                nextHand

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
                        nextPlayer updatedPlayer players

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
                DealerTakesCard_

              else if allHandsHaveTwoCards then
                DealerTakesCard_

              else if allPlayersStandingOrBusted then
                DealerFinish_

              else
                Deal_
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
                            (if List.member state [ Standing, Busted ] then
                                nextHand

                             else
                                identity
                            )
                                ( updatedCurrentHand, rest )
                    }

                updatedPlayers =
                    ( updatedPlayer, players )

                allPlayersStandingOrBusted =
                    allPlayersHaveCond updatedPlayers (\h -> List.member h.state [ Standing, Busted ])

                nextHandHasTwoCards =
                    updatedPlayer.hands |> Tuple.first |> (\h -> List.length h.cards >= 2)
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
                DealerFinish_

              else if not nextHandHasTwoCards then
                TakeCard_

              else
                NoEffect
            )
                |> toastIf (state == Standing && List.length updatedCurrentHand.cards == 2) "Black Jack!"
                |> toastIf (state == Busted) "Bust!"

        Stand ->
            let
                updatedPlayer =
                    { currentPlayer
                        | hands = nextHand <| updateCurrentHand currentPlayer.hands (\h -> { h | state = Standing })
                    }

                -- Can happen when splitting
                nextHandHasTwoCards =
                    updatedPlayer.hands
                        |> Tuple.first
                        |> (\h -> List.length h.cards >= 2)

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
                DealerFinish_

              else if not nextHandHasTwoCards then
                TakeCard_

              else
                NoEffect
            )

        -- Split current hand into two, allow player to take a card/stand/split/doubledown on each hand
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
            ( { model | players = ( updatedPlayer, players ) }, TakeCard_ )

        -- Double bet and take another card
        DoubleDown ->
            let
                currentBet =
                    Tuple.first currentPlayer.hands |> .bet

                ( cards, deck ) =
                    Deck.takeCard model.deck

                ( currentHand, _ ) =
                    currentPlayer.hands

                updatedHand =
                    { currentHand | cards = currentHand.cards ++ cards }

                state =
                    if Hand.largestValue updatedHand.cards > 21 then
                        Busted

                    else
                        Standing

                updatedPlayer =
                    { currentPlayer
                        | hands =
                            nextHand <|
                                updateCurrentHand currentPlayer.hands
                                    (\h ->
                                        { h
                                            | state = state
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

                nextHandHasTwoCards =
                    updatedPlayer.hands
                        |> Tuple.first
                        |> (\h -> List.length h.cards >= 2)
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
                DealerFinish_

              else if not nextHandHasTwoCards then
                TakeCard_

              else
                NoEffect
            )
                |> toastIf (state == Busted) "Bust!"

        -- All players are now finished
        DealerFinish ->
            let
                lowestDealerHandValue =
                    model.dealer |> Hand.largestValue

                dealerHasReachedLimit =
                    lowestDealerHandValue >= 17

                dealerHasBust =
                    lowestDealerHandValue > 21
            in
            if dealerHasReachedLimit || dealerHasBust then
                ( { model | state = Result }, Winnings_ )

            else
                ( model, DealerTakesCard_ )

        -- Dealer has busted or reached 17 now
        Winnings ->
            -- Calculate how many hands won/lost and add money to player
            let
                dealerHandValue =
                    Hand.largestValue model.dealer

                win =
                    calculateWinnings dealerHandValue currentPlayer

                updatedPlayers =
                    ( { currentPlayer | money = currentPlayer.money + calculateWinnings dealerHandValue currentPlayer }
                    , List.map (\p -> { p | money = p.money + calculateWinnings dealerHandValue p }) players
                    )
            in
            ( { model | players = updatedPlayers }, NoEffect )
                |> toast
                    (if win == 0 then
                        "You lost $" ++ String.fromInt (currentPlayer.hands |> playerHands |> List.map .bet |> List.sum) ++ "!"

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
            ( { newState | state = Betting, deck = model.deck, players = cleared }, NoEffect )

        ClearToast ->
            ( { model | toast = Nothing }, NoEffect )



-- VIEWS


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
    Html.div [ Html.Attributes.class "card", Html.Attributes.attribute "test-id" (Card.toString card) ]
        [ Html.div [ Html.Attributes.class "card-inner", Html.Attributes.classList [ ( "hidden", hidden ) ] ]
            [ Html.div [ Html.Attributes.class "color", Html.Attributes.class (Card.suiteToCssClass card) ] []
            , Html.div [ Html.Attributes.class "value", Html.Attributes.class (Card.valueToCssClass card) ] []
            ]
        ]


dealerView : Dealer -> GameState -> Html.Html Msg
dealerView dealer state =
    Html.div [ Html.Attributes.class "dealer" ]
        [ if List.member state [ DealerFinishes, Result ] then
            Html.div [ Html.Attributes.class "cards" ] (List.map (cardView False) dealer)

          else
            case dealer of
                first :: [] ->
                    Html.div [ Html.Attributes.class "cards" ]
                        [ cardView False first
                        ]

                first :: second :: rest ->
                    Html.div [ Html.Attributes.class "cards" ]
                        ([ cardView False first
                         , cardView (state /= Result) second
                         ]
                            ++ List.map (cardView False) rest
                        )

                [] ->
                    Html.text ""
        ]


playerView : Player -> Html.Html Msg
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
                    (\hand ->
                        if hand.order == activeHand.order then
                            activeHandView hand

                        else
                            inactiveHandView hand
                    )
            )
        ]


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
        [ Html.p [ Html.Attributes.class "player-money" ] [ Html.text ("$" ++ String.fromInt player.money) ]
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
        (([ 1, 10, 100, 500 ]
            |> List.map
                (\amount ->
                    Html.div
                        [ Html.Attributes.class "marker"
                        , Html.Attributes.attribute "role" "button"
                        , Html.Attributes.class ("_" ++ String.fromInt amount)
                        , Html.Attributes.classList [ ( "disabled", money < amount ) ]
                        , Html.Events.onClick
                            (if money < amount then
                                NoOp

                             else
                                Bet amount
                            )
                        ]
                        [ Html.text ("$" ++ String.fromInt amount) ]
                )
         )
            ++ (if money > 500 then
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

        -- We will be given another card soon, happens when splitting
        allDisabled =
            List.length cards == 1
    in
    Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
        [ Html.button [ Html.Events.onClick TakeCard, Html.Attributes.disabled (allDisabled || state /= Playing) ] [ Html.text "Hit" ]
        , Html.button [ Html.Events.onClick Stand, Html.Attributes.disabled (allDisabled || state /= Playing) ] [ Html.text "Stand" ]
        , if canSplit cards then
            Html.button [ Html.Events.onClick Split, Html.Attributes.disabled (allDisabled || state /= Playing || money < bet) ] [ Html.text "Split" ]

          else
            Html.text ""
        , if List.length cards == 2 then
            Html.button
                [ Html.Events.onClick DoubleDown
                , Html.Attributes.disabled (money < bet * 2)
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
allHands =
    List.concatMap (.hands >> playerHands)


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
    { player | hands = emptyHands }


canSplit : Hand.Hand -> Bool
canSplit hand =
    case hand of
        [ first, second ] ->
            first.value == second.value

        _ ->
            False


nextPlayer : Player -> List Player -> ( Player, List Player )
nextPlayer currentPlayer rest =
    case rest of
        x :: xs ->
            ( x, xs ++ [ currentPlayer ] )

        _ ->
            ( currentPlayer, rest )


nextHand : ( Hand, List Hand ) -> ( Hand, List Hand )
nextHand ( currentHand, rest ) =
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

            else if Hand.largestValue cards == 21 then
                -- Black Jack, pays 3 to 2
                acc + bet * 3

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


emptyHands : ( Hand, List Hand )
emptyHands =
    ( { cards = [], bet = 0, state = Playing, order = 0 }, [] )


toast : String -> ( Model, Effect ) -> ( Model, Effect )
toast message ( model, effect ) =
    ( { model | toast = Just message }, Multiple [ effect, ClearToast_ ] )


toastIf : Bool -> String -> ( Model, Effect ) -> ( Model, Effect )
toastIf cond message =
    if cond then
        toast message

    else
        identity
