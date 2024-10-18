module DeckTests exposing (suite)

import Card
import Deck
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Deck tests"
        [ test "Correct amount of cards per deck " <|
            \_ -> Deck.decks 1 |> List.length |> Expect.equal 52
        , test "Taking cards takes from front of deck" <|
            \_ ->
                Deck.decks 1
                    |> Deck.takeCards 2
                    |> Tuple.first
                    |> Expect.equalLists [ { suite = Card.Spades, value = Card.Ace }, { suite = Card.Spades, value = Card.Two } ]
        ]
