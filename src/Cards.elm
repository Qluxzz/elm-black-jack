module Cards exposing (canSplit, comp, hasBlackJack, toString, value)

import Card


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


hasBlackJack : List Card.Card -> Bool
hasBlackJack cards =
    List.length cards == 2 && value cards == 21
