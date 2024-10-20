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
                    |> ProgramTest.ensureViewHas
                        (playerHasCards [ Card Card.Ace Card.Spades, Card Card.Three Card.Spades ])
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
                    |> ProgramTest.ensureViewHas
                        (playerHasCards [ Card Card.Ace Card.Spades, Card Card.Three Card.Spades, Card Card.Five Card.Spades ])
                    |> ProgramTest.clickButton "Stand"
                    -- Ensure player won after clicking stand
                    |> ProgramTest.ensureView (playerHasMoney 400)
                    |> ProgramTest.expectViewHas
                        [ Selector.exactText "Continue?"
                        ]
        , test "test splitting works" <|
            \_ ->
                start
                    (defaultSettings
                        |> withDeck [ Card Card.Ten Card.Diamonds, Card Card.Ace Card.Diamonds, Card Card.Ten Card.Spades, Card Card.Ace Card.Diamonds ]
                    )
                    |> ProgramTest.clickButton "$100"
                    -- Validate that the user has the expected cards
                    |> ProgramTest.ensureViewHas
                        (playerHasCards [ Card Card.Ten Card.Diamonds, Card Card.Ten Card.Spades ])
                    |> ProgramTest.clickButton "Split"
                    |> ProgramTest.expectView
                        (Expect.all
                            [ playerHasHand 0 [ Card Card.Ten Card.Diamonds ]
                            , playerHasHand 1 [ Card Card.Ten Card.Spades ]
                            ]
                        )
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
        , test "Double down works" <|
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
                    |> ProgramTest.ensureView (handHasBet 0 100)
                    |> ProgramTest.ensureViewHas
                        (playerHasCards [ Card Card.Five Card.Spades, Card Card.Five Card.Clubs ])
                    |> ProgramTest.clickButton "Double down"
                    |> ProgramTest.ensureView (handHasBet 0 200)
                    |> ProgramTest.ensureViewHas
                        (playerHasCards [ Card Card.Five Card.Spades, Card Card.Five Card.Clubs, Card Card.Ten Card.Diamonds ])
                    |> ProgramTest.ensureView
                        (Expect.all
                            [ playerHasMoney 500
                            , handHasBet 0 200
                            ]
                        )
                    |> ProgramTest.expectViewHas [ Selector.exactText "Continue?" ]
        ]



-- Custom selectors


playerHasCards : List Card.Card -> List Selector.Selector
playerHasCards =
    List.map
        (\card ->
            Selector.all
                [ Selector.exactClassName "player"
                , cardValue card
                ]
        )


cardValue : Card.Card -> Selector.Selector
cardValue card =
    Selector.attribute (Attributes.attribute "test-id" (Card.toString card))


playerHands : Query.Single msg -> Query.Multiple msg
playerHands query =
    query
        |> Query.findAll [ Selector.class "player" ]
        |> Query.first
        |> Query.findAll [ Selector.class "hand" ]


playerHasHand : Int -> List Card -> Query.Single msg -> Expect.Expectation
playerHasHand index cards query =
    query
        |> playerHands
        |> Query.index -index
        |> Query.has
            (List.map
                (\c1 ->
                    Selector.all
                        [ Selector.attribute (Attributes.attribute "test-id" (Card.toString c1))
                        ]
                )
                cards
            )


playerHasMoney : Int -> Query.Single msg -> Expect.Expectation
playerHasMoney amount query =
    query
        |> Query.find [ Selector.class "player-money" ]
        |> Query.has [ Selector.exactText ("$" ++ String.fromInt amount) ]


handHasBet : Int -> Int -> Query.Single msg -> Expect.Expectation
handHasBet index amount query =
    query
        |> playerHands
        |> Query.index -index
        |> Query.has [ Selector.exactText ("$" ++ String.fromInt amount) ]



-- Helpers


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.ShuffleDeck_ deck ->
            SimulatedEffect.Process.sleep 0 |> SimulatedEffect.Task.perform (\_ -> Main.ShuffledDeck deck)

        Main.Deal_ ->
            SimulatedEffect.Process.sleep 0 |> SimulatedEffect.Task.perform (\_ -> Main.Deal)

        Main.DealerTakesCard_ ->
            SimulatedEffect.Process.sleep 0 |> SimulatedEffect.Task.perform (\_ -> Main.DealerTakesCard)

        Main.DealerFinish_ ->
            SimulatedEffect.Process.sleep 0 |> SimulatedEffect.Task.perform (\_ -> Main.DealerFinish)

        Main.Winnings_ ->
            SimulatedEffect.Process.sleep 0 |> SimulatedEffect.Task.perform (\_ -> Main.Winnings)

        Main.ClearToast_ ->
            SimulatedEffect.Process.sleep 0 |> SimulatedEffect.Task.perform (\_ -> Main.ClearToast)

        Main.Multiple effects ->
            SimulatedEffect.Cmd.batch (List.map simulateEffects effects)


type alias Settings =
    { deck : Maybe Deck.Deck
    , players : Maybe ( Main.Player, List Main.Player )
    }


defaultSettings : Settings
defaultSettings =
    { deck = Nothing, players = Just ( { money = 400, hands = Main.emptyHands, order = 0 }, [] ) }


withDeck : Deck.Deck -> Settings -> Settings
withDeck d m =
    { m | deck = Just d }


withPlayers : ( Main.Player, List Main.Player ) -> Settings -> Settings
withPlayers p m =
    { m | players = Just p }


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
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.start ()
        |> ProgramTest.advanceTime 10
