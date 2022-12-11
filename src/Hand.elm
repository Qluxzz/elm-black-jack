module Hand exposing (..)

import Card as Card


type alias Hand =
    List Card.Card


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
