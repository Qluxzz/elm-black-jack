module GameTests exposing (suite)

import Card exposing (Card)
import Deck
import Expect
import Html.Attributes as Attributes
import Main
import ProgramTest
import SimulatedEffect.Cmd
import SimulatedEffect.Process
import SimulatedEffect.Task
import Test exposing (describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test.Test
suite =
    describe "Game loop tests"
        [ test "Game loop works" <|
            \_ ->
                start defaultSettings
                    -- Betting alternatives
                    |> ProgramTest.ensureViewHas
                        [ Selector.exactText "$1"
                        , Selector.exactText "$10"
                        , Selector.exactText "$100"
                        , Selector.exactText "$500"
                        ]
                    |> ProgramTest.clickButton "$100"
                    -- Validate that the user has the expected cards
                    |> ProgramTest.ensureView
                        (handHasCards 0 [ Card Card.Ace Card.Spades, Card Card.Three Card.Spades ])
                    -- Validate that the player money has decreased according to the bet
                    -- And that we're in the hit or stand phase of the game
                    |> ProgramTest.ensureView (playerHasMoney 300)
                    |> ProgramTest.ensureViewHas
                        [ Selector.exactText "Hit"
                        , Selector.exactText "Stand"
                        , Selector.exactText "Double down"
                        ]
                    |> ProgramTest.clickButton "Hit"
                    -- Ensure player has three cards after taking another card
                    |> ProgramTest.ensureView
                        (handHasCards 0 [ Card Card.Ace Card.Spades, Card Card.Three Card.Spades, Card Card.Five Card.Spades ])
                    |> ProgramTest.clickButton "Stand"
                    -- Ensure player won after clicking stand
                    |> ProgramTest.ensureView (playerHasMoney 400)
                    |> ProgramTest.expectViewHas [ continueButton ]
        , describe "Splitting"
            [ test "Allowed if user has cards with equal value and enough funds" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card Card.Eight Card.Diamonds
                                , Card Card.Six Card.Diamonds -- Dealer takes
                                , Card Card.Eight Card.Spades
                                , Card Card.Six Card.Diamonds -- Dealer takes
                                , Card Card.Five Card.Hearts
                                , Card Card.Two Card.Clubs
                                , Card Card.Ten Card.Hearts
                                , Card Card.Ten Card.Diamonds -- Dealer takes and busts (6+6+10 > 21)
                                ]
                        )
                        |> ProgramTest.clickButton "$100"
                        -- Validate that the user has the expected hand
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasBet 0 100
                                , handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Eight Card.Spades ]
                                ]
                            )
                        |> ProgramTest.clickButton "Split"
                        -- The first hand will be given another card automatically
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Five Card.Hearts ]
                                , handHasBet 0 100
                                , handHasCards 1 [ Card Card.Eight Card.Spades ]
                                , handHasBet 1 100
                                ]
                            )
                        -- Now the first hand should be selected to hit/stand/double down with
                        |> ProgramTest.clickButton "Stand"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Five Card.Hearts ]
                                , handHasBet 0 100
                                , handHasCards 1 [ Card Card.Eight Card.Spades, Card Card.Two Card.Clubs ]
                                , handHasBet 0 100
                                ]
                            )
                        -- Now the second hand should be selected to hit/stand/double down with
                        |> ProgramTest.clickButton "Double down"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Five Card.Hearts ]
                                , handHasCards 1 [ Card Card.Eight Card.Spades, Card Card.Two Card.Clubs, Card Card.Ten Card.Hearts ]
                                ]
                            )
                        |> ProgramTest.ensureView (toastHasMessage "You won $600!")
                        |> ProgramTest.ensureView (playerHasMoney 600)
                        |> ProgramTest.expectViewHas [ continueButton ]
            , test "Splitting and getting black jack (21) does not lock up game" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card Card.Ace Card.Diamonds
                                , Card Card.Six Card.Diamonds -- Dealer takes
                                , Card Card.Ace Card.Spades
                                , Card Card.Six Card.Diamonds -- Dealer takes
                                , Card Card.King Card.Hearts
                                , Card Card.Nine Card.Clubs
                                , Card Card.Ten Card.Hearts -- Dealer takes and busts (6+6+10 > 21)
                                ]
                        )
                        |> ProgramTest.clickButton "$100"
                        -- Validate that the user has the expected hand
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasBet 0 100
                                , handHasCards 0 [ Card Card.Ace Card.Diamonds, Card Card.Ace Card.Spades ]
                                ]
                            )
                        |> ProgramTest.clickButton "Split"
                        -- The first hand will be given another card automatically
                        -- We also get black jack so we should continue to the next hand automatically
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Ace Card.Diamonds, Card Card.King Card.Hearts ]
                                , handHasBet 0 100
                                , handHasCards 1 [ Card Card.Ace Card.Spades ]
                                , handHasBet 1 100
                                ]
                            )
                        |> ProgramTest.ensureView (toastHasMessage "Black Jack!")
                        |> ProgramTest.advanceTime 1000
                        -- The second hand is given its second card
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Ace Card.Diamonds, Card Card.King Card.Hearts ]
                                , handHasBet 0 100
                                , handHasCards 1 [ Card Card.Ace Card.Spades, Card Card.Nine Card.Clubs ]
                                , handHasBet 0 100
                                ]
                            )
                        -- Now the second hand should be selected to hit/stand/double down with
                        |> ProgramTest.clickButton "Stand"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Ace Card.Diamonds, Card Card.King Card.Hearts ]
                                , handHasCards 1 [ Card Card.Ace Card.Spades, Card Card.Nine Card.Clubs ]
                                ]
                            )
                        |> ProgramTest.ensureView (playerHasMoney 600)
                        |> ProgramTest.expectViewHas [ continueButton ]
            , test "Splitting and doubling down on first hand does not lock up game" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card Card.Eight Card.Diamonds
                                , Card Card.Six Card.Diamonds -- Dealer takes
                                , Card Card.Eight Card.Spades
                                , Card Card.Six Card.Diamonds -- Dealer takes
                                , Card Card.Ten Card.Hearts
                                , Card Card.Two Card.Clubs
                                , Card Card.Ten Card.Hearts
                                , Card Card.Ten Card.Diamonds -- Dealer takes and busts (6+6+10 > 21)
                                ]
                        )
                        |> ProgramTest.clickButton "$100"
                        -- Validate that the user has the expected hand
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasBet 0 100
                                , handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Eight Card.Spades ]
                                ]
                            )
                        |> ProgramTest.clickButton "Split"
                        -- The first hand will be given another card automatically
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Ten Card.Hearts ]
                                , handHasBet 0 100
                                , handHasCards 1 [ Card Card.Eight Card.Spades ]
                                , handHasBet 1 100
                                ]
                            )
                        -- Now the first hand should be selected to hit/stand/double down with
                        |> ProgramTest.clickButton "Double down"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Ten Card.Hearts, Card Card.Two Card.Clubs ]
                                , handHasBet 0 200
                                , handHasCards 1 [ Card Card.Eight Card.Spades, Card Card.Ten Card.Hearts ]
                                , handHasBet 1 100
                                ]
                            )
                        -- Now the second hand should be selected to hit/stand/double down with
                        |> ProgramTest.clickButton "Stand"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Ten Card.Hearts, Card Card.Two Card.Clubs ]
                                , handHasCards 1 [ Card Card.Eight Card.Spades, Card Card.Ten Card.Hearts ]
                                ]
                            )
                        |> ProgramTest.ensureView (toastHasMessage "You won $600!")
                        |> ProgramTest.ensureView (playerHasMoney 600)
                        |> ProgramTest.expectViewHas [ continueButton ]
            ]
        , test "Can't bet more than you have" <|
            \_ ->
                start
                    (defaultSettings
                        |> withPlayers ( { money = 15, hands = Main.emptyHands, order = 0 }, [] )
                    )
                    |> ProgramTest.ensureViewHas
                        [ Selector.exactText "$15"
                        , Selector.classes [ "marker", "_500", "disabled" ]
                        , Selector.classes [ "marker", "_100", "disabled" ]
                        ]
                    |> ProgramTest.ensureViewHasNot
                        [ Selector.classes [ "marker", "_10", "disabled" ]
                        , Selector.classes [ "marker", "_1", "disabled" ]
                        ]
                    |> ProgramTest.clickButton "$500"
                    |> ProgramTest.expectViewHas [ Selector.exactText "$15" ]
        , describe "Double down"
            [ test "Double down works" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card Card.Five Card.Spades
                                , Card Card.Eight Card.Diamonds
                                , Card Card.Five Card.Clubs
                                , Card Card.Four Card.Hearts
                                , Card Card.Ten Card.Diamonds

                                -- Add another card so dealer busts on 22 (8 + 4 + 10)
                                , Card Card.Ten Card.Diamonds
                                ]
                        )
                        |> ProgramTest.clickButton "$100"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Five Card.Spades, Card Card.Five Card.Clubs ]
                                , handHasBet 0 100
                                ]
                            )
                        |> ProgramTest.clickButton "Double down"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ playerHasMoney 500
                                , handHasBet 0 200
                                , handHasCards 0 [ Card Card.Five Card.Spades, Card Card.Five Card.Clubs, Card Card.Ten Card.Diamonds ]
                                ]
                            )
                        |> ProgramTest.expectViewHas [ continueButton ]
            , test "Show toast if bust when doubling down" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card Card.Ten Card.Spades
                                , Card Card.Eight Card.Diamonds -- Dealer takes
                                , Card Card.Five Card.Clubs
                                , Card Card.Four Card.Hearts -- Dealer takes
                                , Card Card.Ten Card.Diamonds

                                -- Add another card so dealer busts on 22 (8 + 4 + 10)
                                , Card Card.Ten Card.Diamonds
                                ]
                            |> withDelay
                        )
                        |> ProgramTest.clickButton "$100"
                        -- Deal a card per 'tick', so four means the dealer and the player has two cards each
                        |> ProgramTest.advanceTime 4
                        |> ProgramTest.clickButton "Double down"
                        |> ProgramTest.expectView (toastHasMessage "Bust!")
            ]
        , test "Bust if more than 21 in value" <|
            \_ ->
                start
                    (defaultSettings
                        |> withDeck
                            [ Card Card.Ten Card.Diamonds
                            , Card Card.Ten Card.Diamonds
                            , Card Card.Ten Card.Diamonds
                            , Card Card.Ten Card.Diamonds
                            , Card Card.Ten Card.Diamonds
                            , Card Card.Ten Card.Diamonds
                            ]
                    )
                    |> ProgramTest.clickButton "$100"
                    |> ProgramTest.ensureView
                        (Expect.all
                            [ handHasBet 0 100
                            , handHasCards 0 [ Card Card.Ten Card.Diamonds, Card Card.Ten Card.Diamonds ]
                            ]
                        )
                    |> ProgramTest.clickButton "Hit"
                    -- TODO: We're setting multiple toasts here, first one for that we have busted,
                    -- and then directly after one that we lost, should we support multiple toasts that stack or add a delay for the toasts?
                    |> ProgramTest.expectView (toastHasMessage "You lost $100!")
        , test "Dealer second card should be visible before taking third card" <|
            \_ ->
                start
                    (defaultSettings
                        |> withDeck
                            [ Card Card.Ace Card.Diamonds
                            , Card Card.Four Card.Spades -- Dealer
                            , Card Card.Jack Card.Spades
                            , Card Card.Three Card.Hearts -- Dealer
                            , Card Card.Ten Card.Clubs
                            ]
                        |> withDelay
                    )
                    |> ProgramTest.clickButton "$100"
                    |> ProgramTest.advanceTime 3
                    |> ProgramTest.ensureView (toastHasMessage "Black Jack!")
                    |> ProgramTest.advanceTime 1
                    |> ProgramTest.expectView (dealerHasCards [ Card Card.Four Card.Spades, Card Card.Three Card.Hearts ])
        ]



-- Custom selectors


allCards : List Card.Card -> List Selector.Selector
allCards cards =
    List.map (\c1 -> Selector.all [ cardValue c1 ]) cards


cardValue : Card.Card -> Selector.Selector
cardValue card =
    Selector.attribute (Attributes.attribute "test-id" (Card.toString card))


playerHands : Query.Single msg -> Query.Multiple msg
playerHands query =
    query
        |> Query.findAll [ Selector.class "player" ]
        |> Query.first
        |> Query.findAll [ Selector.class "hand" ]


handHasCards : Int -> List Card -> Query.Single msg -> Expect.Expectation
handHasCards index cards query =
    query
        |> playerHands
        |> Query.index -index
        |> Query.has (allCards cards)


playerHasMoney : Int -> Query.Single msg -> Expect.Expectation
playerHasMoney amount query =
    query
        |> Query.find [ Selector.class "player-money" ]
        |> Query.has [ Selector.exactText (toDollars amount) ]


handHasBet : Int -> Int -> Query.Single msg -> Expect.Expectation
handHasBet index amount query =
    query
        |> playerHands
        |> Query.index -index
        |> Query.has [ Selector.exactText (toDollars amount) ]


continueButton : Selector.Selector
continueButton =
    Selector.exactText "Continue?"


toastHasMessage : String -> Query.Single msg -> Expect.Expectation
toastHasMessage message query =
    query
        |> Query.find [ Selector.class "toast" ]
        |> Query.has [ Selector.exactText message ]


dealerHasCards : List Card.Card -> Query.Single msg -> Expect.Expectation
dealerHasCards cards query =
    query
        |> Query.find [ Selector.class "dealer" ]
        |> Query.find [ Selector.class "cards" ]
        |> Expect.all
            [ Query.has (allCards cards)
            , Query.find [ Selector.class "card-inner" ]
                >> Query.hasNot [ Selector.class "hidden" ]
            ]



-- Helpers


toDollars : Int -> String
toDollars amount =
    "$" ++ String.fromInt amount


simulateEffects : Bool -> Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects delay effect =
    let
        setTimeout : Main.Msg -> ProgramTest.SimulatedEffect Main.Msg
        setTimeout msg =
            SimulatedEffect.Process.sleep
                (if delay then
                    1

                 else
                    0
                )
                |> SimulatedEffect.Task.perform (\_ -> msg)
    in
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.ShuffleDeck_ deck ->
            setTimeout (Main.ShuffledDeck deck)

        Main.Deal_ ->
            setTimeout Main.Deal

        Main.DealerTakesCard_ ->
            setTimeout Main.DealerTakesCard

        Main.TakeCard_ ->
            setTimeout Main.TakeCard

        Main.DealerFinish_ _ ->
            setTimeout Main.DealerFinish

        Main.Winnings_ ->
            setTimeout Main.Winnings

        Main.ClearToast_ ->
            -- Set 1 here, so we need to manually advance the time to get rid of the toast
            SimulatedEffect.Process.sleep 1 |> SimulatedEffect.Task.perform (\_ -> Main.ClearToast)

        Main.Multiple effects ->
            SimulatedEffect.Cmd.batch (List.map (simulateEffects delay) effects)


type alias Settings =
    { deck : Maybe Deck.Deck
    , players : Maybe ( Main.Player, List Main.Player )
    , delay : Bool
    }


defaultSettings : Settings
defaultSettings =
    { deck = Nothing
    , players = Just ( { money = 400, hands = Main.emptyHands, order = 0 }, [] )
    , delay = False
    }


withDeck : Deck.Deck -> Settings -> Settings
withDeck d m =
    { m | deck = Just d }


withPlayers : ( Main.Player, List Main.Player ) -> Settings -> Settings
withPlayers p m =
    { m | players = Just p }


{-| Makes it so all Process.sleep takes 1 instead of 0 so you can use ProgramTest.advanceTime to advance through the messages
-}
withDelay : Settings -> Settings
withDelay m =
    { m | delay = True }


start : Settings -> ProgramTest.ProgramTest Main.Model Main.Msg Main.Effect
start settings =
    ProgramTest.createElement
        { init =
            \_ ->
                (case settings.deck of
                    Just d ->
                        Main.initWithDeck d

                    Nothing ->
                        Main.init
                )
                    |> (\( model, effect ) ->
                            case settings.players of
                                Just p ->
                                    ( { model | players = p }, effect )

                                Nothing ->
                                    ( model, effect )
                       )
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.withSimulatedEffects (simulateEffects settings.delay)
        |> ProgramTest.start ()
