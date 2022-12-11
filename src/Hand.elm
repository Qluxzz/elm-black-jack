module Hand exposing (..)

import Card as Card


type alias Hand =
    { cards : List Card.Card
    }


value : Hand -> ( Int, Int )
value { cards } =
    List.foldl
        (\card ->
            \( h1, h2 ) ->
                case Card.value card of
                    Card.Single v ->
                        ( h1 + v, h2 )

                    Card.Double v1 v2 ->
                        ( h1 + v1, h2 + v2 )
        )
        ( 0, 0 )
        cards
