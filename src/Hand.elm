module Hand exposing (Hand, comp, toString, value)

import Card


type alias Hand =
    List Card.Card


{-| Get the value of a hand. A tuple is returned since Ace's is worth 1/11. The first item of the tuple is counting the Ace as 1, the other as 11
-}
value : Hand -> ( Int, Int )
value cards =
    List.foldl
        (\card ->
            \( h1, h2 ) ->
                case Card.value card of
                    Card.Single v ->
                        ( h1 + v, h2 + v )

                    Card.Double v1 v2 ->
                        ( h1 + v1, h2 + v2 )
        )
        ( 0, 0 )
        cards


largestValidHandValue : Hand -> Int
largestValidHandValue hand =
    case value hand of
        ( v1, v2 ) ->
            if v2 > 21 then
                v1

            else
                v2


comp : Hand -> Hand -> Order
comp a b =
    let
        aValue =
            largestValidHandValue a

        bValue =
            largestValidHandValue b
    in
    Basics.compare aValue bValue


toString : Hand -> String
toString hand =
    hand |> List.map Card.toString |> String.join ","
