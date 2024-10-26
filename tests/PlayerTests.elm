module PlayerTests exposing (..)

import Card
import Cards
import Expect
import Player
import Test exposing (Test, describe, test)


type alias Case =
    { hands : ( Player.Hand, List Player.Hand ), dealerHand : List Card.Card, expectedWinnings : Int }


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
      , dealerHand =
            [ Card.Card Card.Ace Card.Diamonds
            , Card.Card Card.Six Card.Spades
            ]
      , expectedWinnings = 300
      }

    -- Both have black jack, push
    , { hands =
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
      , dealerHand =
            [ Card.Card Card.Ace Card.Diamonds
            , Card.Card Card.King Card.Clubs
            ]
      , expectedWinnings = 100
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
      , dealerHand =
            [ Card.Card Card.Ten Card.Clubs
            , Card.Card Card.Nine Card.Diamonds
            ]
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
      , dealerHand =
            [ Card.Card Card.Seven Card.Clubs
            , Card.Card Card.Queen Card.Diamonds
            ]
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
      , dealerHand = List.repeat 2 (Card.Card Card.Ten Card.Diamonds)
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
      , dealerHand =
            [ Card.Card Card.Ace Card.Clubs
            , Card.Card Card.Five Card.Diamonds
            , Card.Card Card.Five Card.Diamonds
            ]
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
      , dealerHand =
            [ Card.Card Card.Nine Card.Clubs
            , Card.Card Card.Jack Card.Spades
            ]

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
      , dealerHand =
            [ Card.Card Card.Nine Card.Clubs
            , Card.Card Card.Jack Card.Spades
            ]

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
      , dealerHand =
            [ Card.Card Card.Five Card.Diamonds
            , Card.Card Card.Ten Card.Spades
            , Card.Card Card.Jack Card.Clubs
            ]
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
      , dealerHand =
            [ Card.Card Card.Five Card.Diamonds
            , Card.Card Card.Ten Card.Spades
            , Card.Card Card.Jack Card.Clubs
            ]
      , expectedWinnings = -100
      }
    ]


suite : Test
suite =
    describe "calculateWinnings"
        (List.map
            (\{ hands, dealerHand, expectedWinnings } ->
                test ("Player hands: " ++ String.join ", " (List.map (.cards >> Cards.toString) (Tuple.first hands :: Tuple.second hands)) ++ ", Dealer hand " ++ Cards.toString dealerHand ++ " = " ++ String.fromInt expectedWinnings) <|
                    \_ ->
                        Player.calculateWinnings dealerHand (Player.Player hands 0 0)
                            |> Expect.equal expectedWinnings
            )
            cases
        )
