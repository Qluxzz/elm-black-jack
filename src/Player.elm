module Player exposing (..)

import Card
import Cards


type HandState
    = Playing
    | Standing
    | Busted


type Insurance
    = NotApplicable
    | Declined
    | Insured Int


type alias Hand =
    { cards : List Card.Card
    , bet : Int
    , state : HandState
    , order : Int
    , insurance : Insurance -- How much we've insured, currently defaults to 50% of the current bet
    }


type alias Player =
    { hands : ( Hand, List Hand )
    , money : Int
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
                    case Basics.compare (Cards.value newCards) 21 of
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
    ( { cards = [], bet = 0, state = Playing, insurance = NotApplicable, order = 0 }, [] )


next : ( x, List x ) -> ( x, List x )
next ( current, rest ) =
    case rest of
        x :: xs ->
            ( x, xs ++ [ current ] )

        _ ->
            ( current, rest )


type HandResult
    = Lost
    | Won
    | Push
    | Blackjack
    | BlackjackPush


calculateHandsState : List Card.Card -> Player -> List HandResult
calculateHandsState dealerHand { hands } =
    let
        dealerHandValue =
            Cards.value dealerHand
    in
    List.map
        (\{ cards, state } ->
            if state == Busted then
                -- You busted, no win
                Lost

            else if Cards.hasBlackjack cards then
                if Cards.hasBlackjack dealerHand then
                    BlackjackPush

                else
                    Blackjack

            else if dealerHandValue > 21 then
                Won

            else
                case Basics.compare (Cards.value cards) dealerHandValue of
                    GT ->
                        Won

                    EQ ->
                        Push

                    LT ->
                        Lost
        )
        (playerHands hands)


calculateWinnings : Int -> HandResult -> Int
calculateWinnings bet state =
    case state of
        Lost ->
            -bet

        Push ->
            bet

        BlackjackPush ->
            bet

        Won ->
            bet * 2

        Blackjack ->
            bet * 3


playerHands : ( Hand, List Hand ) -> List Hand
playerHands ( currentHand, rest ) =
    currentHand :: rest


addToastIfCurrentHandHas : (Hand -> Maybe String) -> Player -> ( Player, Maybe String )
addToastIfCurrentHandHas cond ({ hands } as p) =
    ( p, cond (Tuple.first hands) )
