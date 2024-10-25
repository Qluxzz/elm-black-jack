module Player exposing (..)

import Card
import Cards


type HandState
    = Playing
    | Standing
    | Busted


type alias Hand =
    { cards : List Card.Card
    , bet : Int
    , state : HandState
    , order : Int
    }


type alias Player =
    { hands : ( Hand, List Hand )
    , money : Int
    , order : Int
    }


updateCurrentHand : (Hand -> Hand) -> Player -> Player
updateCurrentHand f ({ hands } as p) =
    let
        ( currentHand, rest ) =
            hands
    in
    { p | hands = ( f currentHand, rest ) }


updatePlayer : (Player -> Player) -> Player -> Player
updatePlayer =
    identity


switchToNextHand : Player -> Player
switchToNextHand ({ hands } as p) =
    { p | hands = next hands }


switchToNextHandIf : (Hand -> Bool) -> Player -> Player
switchToNextHandIf cond ({ hands } as p) =
    if cond (Tuple.first hands) then
        switchToNextHand p

    else
        p


switchToNextPlayer : ( Player, List Player ) -> ( Player, List Player )
switchToNextPlayer =
    next


switchToNextPlayerIf : Bool -> ( Player, List Player ) -> ( Player, List Player )
switchToNextPlayerIf cond =
    if cond then
        switchToNextPlayer

    else
        identity


addCards : List Card.Card -> Player -> Player
addCards cards =
    updateCurrentHand
        (\h ->
            let
                newCards =
                    h.cards ++ cards
            in
            { h
                | cards = newCards
                , state =
                    case Basics.compare (Cards.largestValue newCards) 21 of
                        GT ->
                            Busted

                        EQ ->
                            Standing

                        LT ->
                            Playing
            }
        )


clearHands : Player -> Player
clearHands player =
    { player | hands = emptyHands }


emptyHands : ( Hand, List Hand )
emptyHands =
    ( { cards = [], bet = 0, state = Playing, order = 0 }, [] )


next : ( x, List x ) -> ( x, List x )
next ( current, rest ) =
    case rest of
        x :: xs ->
            ( x, xs ++ [ current ] )

        _ ->
            ( current, rest )


calculateWinnings : Int -> Player -> Int
calculateWinnings dealerHand { hands } =
    List.foldr
        (\{ cards, bet, state } acc ->
            if state == Busted then
                -- You busted, no win
                acc - bet

            else if dealerHand > 21 then
                -- Dealer busted, automatic win
                acc + bet * 2

            else if List.length cards == 2 && Cards.largestValue cards == 21 then
                -- Black Jack, pays 3 to 2
                acc + bet * 3

            else
                case Basics.compare (Cards.largestValue cards) dealerHand of
                    GT ->
                        -- You won over the dealer, you get 2x your bet back
                        acc + bet * 2

                    EQ ->
                        -- Push, you get your inital bet back
                        acc + bet

                    LT ->
                        -- You lost to the dealer
                        acc - bet
        )
        0
        (playerHands hands)


playerHands : ( Hand, List Hand ) -> List Hand
playerHands ( currentHand, rest ) =
    currentHand :: rest


addToastIfCurrentHandHas : (Hand -> Maybe String) -> Player -> ( Player, Maybe String )
addToastIfCurrentHandHas cond ({ hands } as p) =
    ( p, cond (Tuple.first hands) )
