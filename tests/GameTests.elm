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
import Test.Html.Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test.Test
suite =
    describe "Game loop tests"
        [ test "Game loop works" <|
            \_ ->
                start (defaultSettings |> withDeck (Deck.decks 4))
                    -- Betting alternatives
                    |> ProgramTest.ensureViewHas
                        [ Selector.exactText "$1"
                        , Selector.exactText "$10"
                        , Selector.exactText "$100"
                        , Selector.exactText "$500"
                        , Selector.exactText "$1000"
                        ]
                    |> clickMarker 100
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
                    -- Continue to next round
                    |> ProgramTest.clickButton "Continue?"
                    |> clickMarker 100
                    -- Validate that the user has the expected cards
                    |> ProgramTest.ensureView
                        (handHasCards 0 [ Card Card.Eight Card.Spades, Card Card.Ten Card.Spades ])
                    -- Validate that the player money has decreased according to the bet
                    -- And that we're in the hit or stand phase of the game
                    |> ProgramTest.ensureView (playerHasMoney 300)
                    |> ProgramTest.clickButton "Hit"
                    -- Ensure player has three cards after taking another card
                    |> ProgramTest.ensureView
                        (handHasCards 0 [ Card Card.Eight Card.Spades, Card Card.Ten Card.Spades, Card Card.Queen Card.Spades ])
                    -- Ensure player lost because hand was over 21
                    |> ProgramTest.ensureView (playerHasMoney 300)
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
                        |> clickMarker 100
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
                        |> ProgramTest.ensureView (playerHasMoney 700)
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
                        |> clickMarker 100
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
                        |> ProgramTest.ensureView (playerHasMoney 700)
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
                        |> clickMarker 100
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
                        |> ProgramTest.ensureView (playerHasMoney 700)
                        |> ProgramTest.expectViewHas [ continueButton ]
            , test "Splitting hands keeps the hands in the expected order" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card Card.Eight Card.Diamonds -- Player gets
                                , Card Card.Two Card.Clubs -- Dealer takes
                                , Card Card.Eight Card.Hearts -- Player gets
                                , Card Card.Two Card.Hearts -- Dealer takes
                                , Card Card.Eight Card.Spades -- Player splits, get on first hand
                                , Card Card.Five Card.Clubs -- Player splits again, get on first hand
                                , Card Card.Eight Card.Spades -- Player splits second hand
                                , Card Card.Ten Card.Diamonds -- Player gets second card on second hand
                                ]
                        )
                        |> clickMarker 100
                        |> ProgramTest.ensureView
                            (handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Eight Card.Hearts ])
                        |> ProgramTest.clickButton "Split"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Eight Card.Spades ]
                                , handHasCards 1 [ Card Card.Eight Card.Hearts ]
                                ]
                            )
                        |> ProgramTest.clickButton "Split"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Five Card.Clubs ]
                                , handHasCards 1 [ Card Card.Eight Card.Spades ]
                                , handHasCards 2 [ Card Card.Eight Card.Hearts ]
                                ]
                            )
                        |> ProgramTest.clickButton "Stand"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Five Card.Clubs ]
                                , handHasCards 1 [ Card Card.Eight Card.Spades, Card Card.Eight Card.Spades ]
                                , handHasCards 2 [ Card Card.Eight Card.Hearts ]
                                ]
                            )
                        |> ProgramTest.clickButton "Split"
                        |> ProgramTest.expectView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Eight Card.Diamonds, Card Card.Five Card.Clubs ]
                                , handHasCards 1 [ Card Card.Eight Card.Spades, Card Card.Ten Card.Diamonds ]
                                , handHasCards 2 [ Card Card.Eight Card.Spades ]
                                , handHasCards 3 [ Card Card.Eight Card.Hearts ]
                                ]
                            )
            ]
        , test "Can't bet more than you have" <|
            \_ ->
                start
                    (defaultSettings
                        |> withPlayers ( { money = 15 }, [] )
                        |> withDeck []
                    )
                    |> ProgramTest.ensureViewHas
                        [ Selector.exactText "Balance: $15"
                        , Selector.disabled True
                        , Selector.classes [ "marker", "_500" ]
                        , Selector.classes [ "marker", "_100" ]
                        ]
                    |> ProgramTest.expectViewHas
                        [ Selector.disabled True
                        , Selector.classes [ "marker", "_10" ]
                        , Selector.classes [ "marker", "_1" ]
                        ]
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
                        |> clickMarker 100
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ handHasCards 0 [ Card Card.Five Card.Spades, Card Card.Five Card.Clubs ]
                                , handHasBet 0 100
                                ]
                            )
                        |> ProgramTest.clickButton "Double down"
                        |> ProgramTest.ensureView
                            (Expect.all
                                [ playerHasMoney 600
                                , handHasBet 0 200
                                , handHasCards 0 [ Card Card.Five Card.Spades, Card Card.Five Card.Clubs, Card Card.Ten Card.Diamonds ]
                                ]
                            )
                        |> ProgramTest.expectViewHas [ continueButton ]
            , test "Doubling down is allowed if the player has enough money to do it, money >= bet" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withPlayers ( { money = 1000 }, [] )
                            |> withDelay
                            |> withDeck
                                [ Card Card.Ten Card.Spades
                                , Card Card.Ace Card.Spades -- Dealer takes
                                , Card Card.Two Card.Diamonds
                                , Card Card.Five Card.Clubs -- Dealer takes
                                , Card Card.Five Card.Hearts
                                , Card Card.Ten Card.Clubs -- Dealer takes
                                , Card Card.Ten Card.Clubs -- Dealer takes
                                ]
                        )
                        |> ProgramTest.clickButton "$500"
                        |> ProgramTest.advanceTime 4
                        |> ProgramTest.clickButton "Double down"
                        |> ProgramTest.ensureView (playerHasMoney 0)
                        |> ProgramTest.advanceTime 6
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
                        |> clickMarker 100
                        -- Deal a card per 'tick', so four means the dealer and the player has two cards each
                        |> ProgramTest.advanceTime 4
                        |> ProgramTest.clickButton "Double down"
                        |> ProgramTest.ensureView (handHasBusted 0)
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
                        |> withDelay
                    )
                    |> clickMarker 100
                    -- Deal a card per 'tick', so four means the dealer and the player has two cards each
                    |> ProgramTest.advanceTime 4
                    |> ProgramTest.ensureView
                        (Expect.all
                            [ handHasBet 0 100
                            , handHasCards 0 [ Card Card.Ten Card.Diamonds, Card Card.Ten Card.Diamonds ]
                            ]
                        )
                    |> ProgramTest.clickButton "Hit"
                    |> ProgramTest.expectView (toastHasMessage "Bust!")
        , describe "Dealer"
            [ test "Dealer second card should be visible before taking third card" <|
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
                        |> clickMarker 100
                        -- Deal a card per 'tick', so three means the dealer has one card and the player has two cards
                        |> ProgramTest.advanceTime 3
                        |> ProgramTest.ensureView (toastHasMessage "Black Jack!")
                        -- Make the dealer take the last card
                        |> ProgramTest.advanceTime 2
                        |> ProgramTest.ensureView (dealerHasCards [ Card Card.Four Card.Spades, Card Card.Three Card.Hearts ])
                        |> ProgramTest.advanceTime 3
                        |> ProgramTest.ensureView (toastHasMessage "You won $300!")
                        |> ProgramTest.ensureView (dealerHasCards [ Card Card.Four Card.Spades, Card Card.Three Card.Hearts ])
                        |> ProgramTest.expectViewHas [ continueButton ]
            , test "Dealer stands on soft 17" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card Card.Three Card.Diamonds
                                , Card Card.Ace Card.Hearts -- Dealer card
                                , Card Card.Four Card.Diamonds
                                , Card Card.Six Card.Spades

                                -- Extra cards the dealer should not take
                                , Card Card.Five Card.Diamonds
                                , Card Card.Ten Card.Hearts
                                ]
                            |> withDelay
                        )
                        |> clickMarker 100
                        -- Deal a card per 'tick', so four means the dealer and the player has two cards each
                        |> ProgramTest.advanceTime 4
                        |> ProgramTest.clickButton "Stand"
                        |> ProgramTest.advanceTime 1
                        |> ProgramTest.ensureView (dealerHasCards [ Card Card.Ace Card.Hearts, Card Card.Six Card.Spades ])
                        |> ProgramTest.advanceTime 1
                        |> ProgramTest.expectViewHas [ continueButton ]
            ]
        , describe "Statistics"
            [ test "Statistics is updated as expected" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck [ Card.Card Card.Ace Card.Spades, Card.Card Card.Ten Card.Clubs, Card.Card Card.King Card.Diamonds, Card.Card Card.Seven Card.Hearts ]
                        )
                        |> clickMarker 100
                        |> ProgramTest.clickButton "Quit?"
                        |> ProgramTest.clickButton "Statistics"
                        |> ProgramTest.expectViewHas
                            [ Selector.exactText "Rounds played: 1"
                            , Selector.exactText "Hands won: 1"
                            , Selector.exactText "Hands lost: 0"
                            , Selector.exactText "Win rate: 100%"
                            , Selector.exactText "Blackjack: 1"
                            , Selector.exactText "Blackjack push: 0"
                            , Selector.exactText "Double down: 0"
                            , Selector.exactText "Biggest hand won: $300"
                            , Selector.exactText "Biggest hand lost: $0"
                            , Selector.exactText "Most hands in a round: 1"
                            ]
            , test "Splitting is counted correctly in statistics" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card.Card Card.Ace Card.Spades
                                , Card.Card Card.Ten Card.Clubs
                                , Card.Card Card.Ace Card.Diamonds
                                , Card.Card Card.Seven Card.Hearts
                                , Card.Card Card.King Card.Clubs
                                , Card.Card Card.Queen Card.Diamonds
                                ]
                        )
                        |> clickMarker 100
                        |> ProgramTest.clickButton "Split"
                        |> ProgramTest.clickButton "Quit?"
                        |> ProgramTest.clickButton "Statistics"
                        |> ProgramTest.expectViewHas
                            [ Selector.exactText "Rounds played: 1"
                            , Selector.exactText "Hands won: 2"
                            , Selector.exactText "Hands lost: 0"
                            , Selector.exactText "Win rate: 100%"
                            , Selector.exactText "Blackjack: 2"
                            , Selector.exactText "Blackjack push: 0"
                            , Selector.exactText "Double down: 0"
                            , Selector.exactText "Biggest hand won: $300"
                            , Selector.exactText "Biggest hand lost: $0"
                            , Selector.exactText "Most hands in a round: 2"
                            ]
            , test "Double down is counted correctly in statistics" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card.Card Card.Three Card.Spades
                                , Card.Card Card.Ten Card.Clubs
                                , Card.Card Card.Seven Card.Diamonds
                                , Card.Card Card.Seven Card.Hearts
                                , Card.Card Card.King Card.Clubs
                                ]
                        )
                        |> clickMarker 100
                        |> ProgramTest.clickButton "Double down"
                        |> ProgramTest.clickButton "Quit?"
                        |> ProgramTest.clickButton "Statistics"
                        |> ProgramTest.expectViewHas
                            [ Selector.exactText "Rounds played: 1"
                            , Selector.exactText "Hands won: 1"
                            , Selector.exactText "Hands lost: 0"
                            , Selector.exactText "Win rate: 100%"
                            , Selector.exactText "Blackjack: 0"
                            , Selector.exactText "Blackjack push: 0"
                            , Selector.exactText "Double down: 1"
                            , Selector.exactText "Biggest hand won: $400"
                            , Selector.exactText "Biggest hand lost: $0"
                            , Selector.exactText "Most hands in a round: 1"
                            ]
            , test "Blackjack push is counted correctly in statistics" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card.Card Card.Ace Card.Spades
                                , Card.Card Card.Ace Card.Clubs
                                , Card.Card Card.King Card.Diamonds
                                , Card.Card Card.King Card.Clubs
                                ]
                        )
                        |> clickMarker 100
                        |> ProgramTest.clickButton "Quit?"
                        |> ProgramTest.clickButton "Statistics"
                        |> ProgramTest.expectViewHas
                            [ Selector.exactText "Rounds played: 1"
                            , Selector.exactText "Hands won: 0"
                            , Selector.exactText "Hands lost: 0"
                            , Selector.exactText "Win rate: 0%"
                            , Selector.exactText "Blackjack: 1"
                            , Selector.exactText "Blackjack push: 1"
                            , Selector.exactText "Push: 0"
                            , Selector.exactText "Double down: 0"
                            , Selector.exactText "Biggest hand won: $100"
                            , Selector.exactText "Biggest hand lost: $0"
                            , Selector.exactText "Most hands in a round: 1"
                            ]
            , test "Push is counted correctly in statistics" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card.Card Card.Ten Card.Spades
                                , Card.Card Card.Ten Card.Clubs
                                , Card.Card Card.Ten Card.Diamonds
                                , Card.Card Card.Ten Card.Clubs
                                ]
                        )
                        |> clickMarker 100
                        |> ProgramTest.clickButton "Stand"
                        |> ProgramTest.clickButton "Quit?"
                        |> ProgramTest.clickButton "Statistics"
                        |> ProgramTest.expectViewHas
                            [ Selector.exactText "Rounds played: 1"
                            , Selector.exactText "Hands won: 0"
                            , Selector.exactText "Hands lost: 0"
                            , Selector.exactText "Win rate: 0%"
                            , Selector.exactText "Blackjack: 0"
                            , Selector.exactText "Blackjack push: 0"
                            , Selector.exactText "Push: 1"
                            , Selector.exactText "Double down: 0"
                            , Selector.exactText "Biggest hand won: $100"
                            , Selector.exactText "Biggest hand lost: $0"
                            , Selector.exactText "Most hands in a round: 1"
                            ]
            , test "Statistics is persisted over multiple rounds" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck
                                [ Card.Card Card.King Card.Spades
                                , Card.Card Card.Ten Card.Clubs
                                , Card.Card Card.King Card.Diamonds
                                , Card.Card Card.Seven Card.Hearts

                                -- End of round one
                                , Card.Card Card.Five Card.Diamonds
                                , Card.Card Card.Ace Card.Hearts
                                , Card.Card Card.Ten Card.Spades
                                , Card.Card Card.Jack Card.Clubs
                                ]
                        )
                        |> clickMarker 100
                        |> ProgramTest.clickButton "Stand"
                        |> ProgramTest.clickButton "Continue?"
                        |> clickMarker 100
                        |> ProgramTest.clickButton "Stand"
                        |> ProgramTest.clickButton "Quit?"
                        |> ProgramTest.clickButton "Statistics"
                        |> ProgramTest.expectViewHas
                            [ Selector.exactText "Rounds played: 2"
                            , Selector.exactText "Hands won: 1"
                            , Selector.exactText "Hands lost: 1"
                            , Selector.exactText "Win rate: 50%"
                            , Selector.exactText "Blackjack: 0"
                            , Selector.exactText "Blackjack push: 0"
                            , Selector.exactText "Double down: 0"
                            , Selector.exactText "Biggest hand won: $200"
                            , Selector.exactText "Biggest hand lost: $100"
                            , Selector.exactText "Most hands in a round: 1"
                            ]
            , test "Most player money in a game is updated correctly" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withDeck [ Card.Card Card.Ace Card.Spades, Card.Card Card.Ten Card.Clubs, Card.Card Card.King Card.Diamonds, Card.Card Card.Seven Card.Hearts ]
                        )
                        |> clickMarker 100
                        |> ProgramTest.clickButton "Quit?"
                        |> ProgramTest.clickButton "Statistics"
                        |> ProgramTest.expectViewHas
                            [ Selector.exactText "Highest balance: $600"
                            ]
            , test "Win rate is formatted correctly (rounded to two decimals if not a whole number)" <|
                \_ ->
                    start
                        (defaultSettings
                            |> withStatistics (Main.defaultStatistics |> (\s -> { s | wins = 1, loses = 2 }))
                        )
                        |> ProgramTest.clickButton "Statistics"
                        |> ProgramTest.expectViewHas
                            [ Selector.exactText "Win rate: 33.33%"
                            ]
            ]
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
        |> Query.index index
        |> Query.has (allCards cards)


playerHasMoney : Int -> Query.Single msg -> Expect.Expectation
playerHasMoney amount query =
    query
        |> Query.find [ Selector.class "player-money" ]
        |> Query.has [ Selector.exactText ("Balance: " ++ Main.toDollars amount) ]


handHasBet : Int -> Int -> Query.Single msg -> Expect.Expectation
handHasBet index amount query =
    query
        |> playerHands
        |> Query.index index
        |> Query.has [ Selector.exactText (Main.toDollars amount) ]


handHasBusted : Int -> Query.Single msg -> Expect.Expectation
handHasBusted index query =
    query
        |> playerHands
        |> Query.index index
        |> Query.has [ Selector.class "busted" ]


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
            , Query.hasNot [ Selector.class "hidden" ]
            ]



-- Helpers


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

        Main.DealerFinish_ ->
            setTimeout Main.DealerFinish

        Main.Winnings_ ->
            setTimeout Main.Winnings

        Main.ClearToast_ ->
            -- Set 1 here, so we need to manually advance the time to get rid of the toast
            SimulatedEffect.Process.sleep 1 |> SimulatedEffect.Task.perform (\_ -> Main.ClearToast)

        Main.UpdateStatistics _ ->
            SimulatedEffect.Cmd.none

        Main.Multiple effects ->
            SimulatedEffect.Cmd.batch (List.map (simulateEffects delay) effects)


type alias Settings =
    { deck : Maybe Deck.Deck
    , players : Maybe ( { money : Int }, List { money : Int } )
    , delay : Bool
    , highScore : Maybe Int
    , statistics : Maybe Main.Statistics
    }


defaultSettings : Settings
defaultSettings =
    { deck = Nothing
    , players = Just ( { money = 400 }, [] )
    , delay = False
    , highScore = Nothing
    , statistics = Nothing
    }


withDeck : Deck.Deck -> Settings -> Settings
withDeck d s =
    { s | deck = Just d }


withPlayers : ( { money : Int }, List { money : Int } ) -> Settings -> Settings
withPlayers p s =
    { s | players = Just p }


{-| Makes it so all Process.sleep takes 1 instead of 0 so you can use ProgramTest.advanceTime to advance through the messages
-}
withDelay : Settings -> Settings
withDelay s =
    { s | delay = True }


withStatistics : Main.Statistics -> Settings -> Settings
withStatistics statistics s =
    { s | statistics = Just statistics }


start : Settings -> ProgramTest.ProgramTest Main.Model Main.Msg Main.Effect
start settings =
    ProgramTest.createDocument
        { init =
            \_ ->
                (case settings.deck of
                    Just d ->
                        Main.initWithDeck d

                    Nothing ->
                        Main.init
                            { statistics = settings.statistics
                            }
                )
                    |> (\( model, effect ) ->
                            case settings.players of
                                Just p ->
                                    ( model, effect )
                                        |> Main.withPlayers p

                                Nothing ->
                                    ( model, effect )
                       )
        , update = Main.update
        , view = \model -> { title = "Black Jack", body = Main.view model }
        }
        |> ProgramTest.withSimulatedEffects (simulateEffects settings.delay)
        |> ProgramTest.start ()



{- Click on a marker by marker amount

   Required since ProgramTest.clickButton "$100" will match both buttons "$100" and "$1000".

   The following doesn't work either:
   ```elm
       ProgramTest.simulateDomEvent
        (Query.find [ Selector.tag "button", Selector.exactText ("$" ++ String.fromInt amount) ])
        Test.Html.Event.click
   ```

   Since the query returns the text node, and not the button itself, and we can't click a text node

-}


clickMarker : Int -> ProgramTest.ProgramTest model msg effect -> ProgramTest.ProgramTest model msg effect
clickMarker amount =
    ProgramTest.simulateDomEvent
        (Query.find
            [ Selector.tag "button"
            , Selector.disabled False
            , Selector.classes [ "marker", "_" ++ String.fromInt amount ]
            ]
        )
        Test.Html.Event.click
