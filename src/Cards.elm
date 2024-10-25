module Cards exposing (canSplit, comp, largestValue, toString)

import Card


{-| Get the value of a hand. A tuple is returned since Aces are worth 1/11. The first item of the tuple is counting the Ace as 1, the other as 11
-}
value : List Card.Card -> ( Int, Int )
value cards =
    List.foldl
        (\card ( h1, h2 ) ->
            case Card.value card of
                Card.Single v ->
                    ( h1 + v, h2 + v )

                Card.Double v1 v2 ->
                    ( h1 + v1, h2 + v2 )
        )
        ( 0, 0 )
        cards


{-| Aces are counted as 1/11 so if counting aces as 11 gives us a value over 21 (bust), then return value where we count aces as 1 instead
-}
largestValue : List Card.Card -> Int
largestValue cards =
    let
        ( v1, v2 ) =
            value cards
    in
    if v2 > 21 then
        v1

    else
        v2


comp : List Card.Card -> List Card.Card -> Order
comp a b =
    Basics.compare (largestValue a) (largestValue b)


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
