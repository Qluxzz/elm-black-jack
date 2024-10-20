module Hand exposing (Hand, comp, largestValue, toString)

import Card


type alias Hand =
    List Card.Card


{-| Get the value of a hand. A tuple is returned since Aces are worth 1/11. The first item of the tuple is counting the Ace as 1, the other as 11
-}
value : Hand -> ( Int, Int )
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


largestValue : Hand -> Int
largestValue hand =
    let
        ( v1, v2 ) =
            value hand
    in
    if v2 > 21 then
        v1

    else
        v2


comp : Hand -> Hand -> Order
comp a b =
    Basics.compare (largestValue a) (largestValue b)


toString : Hand -> String
toString hand =
    hand |> List.map Card.toString |> String.join ","
