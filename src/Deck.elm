module Deck exposing (Deck, decks, takeCard, takeCards)

import Card exposing (..)


type alias Deck =
    List Card


decks : Int -> Deck
decks amount =
    List.range 1 amount
        |> List.foldr (\_ -> \acc -> acc ++ newDeck) []


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


takeCards : Deck -> Int -> ( List Card, Deck )
takeCards cards amount =
    ( List.take amount cards, List.drop amount cards )


takeCard : Deck -> ( List Card, Deck )
takeCard cards =
    takeCards cards 1
