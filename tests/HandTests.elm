module HandTests exposing (suite)

import Card
import Expect
import Hand
import Test exposing (Test, describe, test)


cases : List ( ( Hand.Hand, Hand.Hand ), Order )
cases =
    [ ( ( [ card Card.Ace, card Card.King ], [ card Card.Ace, card Card.King ] ), EQ )
    , ( ( [ card Card.Two, card Card.Three ], [ card Card.Two, card Card.Five ] ), LT )
    , ( ( [ card Card.Two, card Card.Six ], [ card Card.Two, card Card.Five ] ), GT )
    , ( ( [ card Card.Ace ], [ card Card.Ten ] ), GT )
    ]


suite : Test
suite =
    describe "Hand tests"
        (List.map
            (\( ( h1, h2 ), expected ) ->
                test (Hand.toString h1 ++ " " ++ Hand.toString h2) <|
                    \_ -> Hand.comp h1 h2 |> Expect.equal expected
            )
            cases
        )



-- HELPERS


createCard : Card.Suit -> Card.Value -> Card.Card
createCard s v =
    Card.Card v s


card : Card.Value -> Card.Card
card =
    createCard Card.Clubs
