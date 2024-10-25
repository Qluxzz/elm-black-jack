module PlayerTests exposing (..)

import Card
import Cards
import Expect
import Player
import Test exposing (Test, describe, test)


type alias Case =
    { hands : ( Player.Hand, List Player.Hand ), dealerHandValue : Int, expectedWinnings : Int }


cases : List Case
cases =
    -- Black Jack
    [ { hands =
            ( { bet = 100
              , cards =
                    [ Card.Card Card.Ace Card.Diamonds
                    , Card.Card Card.King Card.Clubs
                    ]
              , state = Player.Standing
              , order = 0
              }
            , []
            )
      , dealerHandValue = 17
      , expectedWinnings = 300
      }

    -- Regular win
    , { hands =
            ( { bet = 100
              , cards = List.repeat 2 (Card.Card Card.Ten Card.Diamonds)
              , state = Player.Standing
              , order = 0
              }
            , []
            )
      , dealerHandValue = 19
      , expectedWinnings = 200
      }

    -- Busted
    , { hands =
            ( { bet = 100
              , cards = List.repeat 3 (Card.Card Card.Ten Card.Diamonds)
              , state = Player.Busted
              , order = 0
              }
            , []
            )
      , dealerHandValue = 17
      , expectedWinnings = -100
      }

    -- Push
    , { hands =
            ( { bet = 100
              , cards = List.repeat 2 (Card.Card Card.Ten Card.Diamonds)
              , state = Player.Standing
              , order = 0
              }
            , []
            )
      , dealerHandValue = 20
      , expectedWinnings = 100
      }

    -- Regular lose
    , { hands =
            ( { bet = 100
              , cards = List.repeat 2 (Card.Card Card.Ten Card.Diamonds)
              , state = Player.Standing
              , order = 0
              }
            , []
            )
      , dealerHandValue = 21
      , expectedWinnings = -100
      }

    -- Two hands, one won and one lose
    , { hands =
            ( { bet = 100
              , cards = List.repeat 2 (Card.Card Card.Ten Card.Diamonds)
              , state = Player.Standing
              , order = 0
              }
            , [ { bet = 100
                , cards = [ Card.Card Card.Ten Card.Diamonds, Card.Card Card.Five Card.Clubs ]
                , state = Player.Standing
                , order = 0
                }
              ]
            )
      , dealerHandValue = 19

      -- We have bet 200 dollars, we lose the 100 on one hand, and win 100 (in addition to our bet) on the other
      , expectedWinnings = 100
      }

    -- Two hands, one double down won and one regular won
    , { hands =
            ( { bet = 200
              , cards = [ Card.Card Card.Ten Card.Diamonds, Card.Card Card.Two Card.Hearts, Card.Card Card.Nine Card.Spades ]
              , state = Player.Standing
              , order = 0
              }
            , [ { bet = 100
                , cards = List.repeat 2 (Card.Card Card.Ten Card.Diamonds)
                , state = Player.Standing
                , order = 0
                }
              ]
            )
      , dealerHandValue = 19

      -- We have bet 300 dollars, we get 200 * 2, and 100 * 2 back
      , expectedWinnings = 600
      }

    -- Dealer busts
    , { hands =
            ( { bet = 100
              , cards = List.repeat 2 (Card.Card Card.Two Card.Diamonds)
              , state = Player.Standing
              , order = 0
              }
            , []
            )
      , dealerHandValue = 25
      , expectedWinnings = 200
      }

    -- Dealer and player busts
    , { hands =
            ( { bet = 100
              , cards = List.repeat 3 (Card.Card Card.Ten Card.Diamonds)
              , state = Player.Busted
              , order = 0
              }
            , []
            )
      , dealerHandValue = 25
      , expectedWinnings = -100
      }
    ]


suite : Test
suite =
    describe "calculateWinnings"
        (List.map
            (\{ hands, dealerHandValue, expectedWinnings } ->
                test ("Player hands: " ++ String.join ", " (List.map (.cards >> Cards.toString) (Tuple.first hands :: Tuple.second hands)) ++ ", Dealer hand " ++ String.fromInt dealerHandValue ++ " = " ++ String.fromInt expectedWinnings) <|
                    \_ ->
                        Player.calculateWinnings dealerHandValue (Player.Player hands 0 0)
                            |> Expect.equal expectedWinnings
            )
            cases
        )
