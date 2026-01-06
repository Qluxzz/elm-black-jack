module CardsTests exposing (..)

import Card
import Cards
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "largestHandValue"
        [ test "Multiple aces can be counted as both 11 and 1 in same hand" <|
            \_ ->
                Cards.values [ Card.Card Card.Ace Card.Diamonds ]
                    |> Expect.equal (Cards.LowHigh 1 11)
        , test "Triple aces is counted as 13" <|
            \_ ->
                Cards.values (List.repeat 3 (Card.Card Card.Ace Card.Diamonds))
                    |> Expect.equal (Cards.LowHigh 3 33)
        ]
