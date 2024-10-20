module Deck exposing (Deck, decks, takeCard, takeCards)

import Card exposing (Card, Suit(..), Value(..))


type alias Deck =
    List Card


decks : Int -> Deck
decks amount =
    List.repeat amount newDeck
        |> List.concat


suites : List Card.Suit
suites =
    [ Clubs, Diamonds, Hearts, Spades ]


values : List Card.Value
values =
    [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]


newDeck : Deck
newDeck =
    List.foldr
        (\suite acc ->
            acc ++ List.map (\v -> Card.Card v suite) values
        )
        []
        suites


takeCards : Int -> Deck -> ( List Card, Deck )
takeCards amount cards =
    ( List.take amount cards, List.drop amount cards )


takeCard : Deck -> ( List Card, Deck )
takeCard =
    takeCards 1
