module Deck exposing (..)

import Card exposing (..)


type alias Deck =
    List Card


newDeck : Deck
newDeck =
    let
        suites : List Card.Suit
        suites =
            [ Clubs, Diamonds, Hearts, Spades ]

        values : List Card.Value
        values =
            [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]
    in
    List.foldr
        (\suite ->
            \acc ->
                acc ++ List.map (\v -> Card.Card suite v) values
        )
        []
        suites


takeCard : Deck -> ( Card, Deck )
takeCard cards =
    case cards of
        first :: rest ->
            ( first, rest )

        [] ->
            Debug.todo "Empty deck!"
