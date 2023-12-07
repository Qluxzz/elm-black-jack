module DeckTests exposing (..)

import Card
import Deck
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Deck tests"
        [ test "Creating a new deck works" <|
            \_ ->
                Deck.decks 1
                    |> List.take 2
                    |> Expect.equalLists [ { suite = Card.Spades, value = Card.Ace }, { suite = Card.Spades, value = Card.Two } ]
        , test "Correct amount of cards per deck " <|
            \_ -> Deck.decks 1 |> List.length |> Expect.equal 52
        ]
