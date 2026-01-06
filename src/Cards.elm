module Cards exposing (HandValue(..), canSplit, comp, hasBlackjack, toString, toString2, value, values)

import Card


type HandValue
    = LowHigh Int Int


{-| Get both high and low values of the cards. So in this case two aces will be 2 and not 22
-}
values : List Card.Card -> HandValue
values cards =
    List.foldr
        (\card (LowHigh low high) ->
            case Card.value card of
                Card.Double low_ high_ ->
                    LowHigh (low + low_) (high + high_)

                Card.Single v ->
                    LowHigh (low + v) (high + v)
        )
        (LowHigh 0 0)
        cards


{-| Get the highest valid value of the cards
-}
value : List Card.Card -> Int
value cards =
    let
        -- Sum the cards without double value first
        nonAcesSum =
            List.foldr
                (\card acc ->
                    case Card.value card of
                        Card.Single v ->
                            acc + v

                        _ ->
                            acc
                )
                0
                cards
    in
    -- Add the aces as 1/11 depending on what works without busting the hand
    List.foldr
        (\card acc ->
            case Card.value card of
                Card.Double low high ->
                    if acc + high > 21 then
                        acc + low

                    else
                        acc + high

                _ ->
                    acc
        )
        nonAcesSum
        cards


comp : List Card.Card -> List Card.Card -> Order
comp a b =
    Basics.compare (value a) (value b)


toString : List Card.Card -> String
toString cards =
    cards |> List.map Card.toString |> String.join ","


canSplit : List Card.Card -> Bool
canSplit cards =
    case cards of
        [ first, second ] ->
            first.value == second.value

        _ ->
            False


hasBlackjack : List Card.Card -> Bool
hasBlackjack cards =
    List.length cards == 2 && value cards == 21


toString2 : HandValue -> String
toString2 (LowHigh low high) =
    if low == high || high > 21 then
        String.fromInt low

    else
        String.fromInt low ++ "/" ++ String.fromInt high
