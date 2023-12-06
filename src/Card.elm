module Card exposing (..)


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Value
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type alias Card =
    { suite : Suit
    , value : Value
    }


type CardValue
    = Single Int
    | Double Int Int


value : Card -> CardValue
value card =
    case card.value of
        Ace ->
            Double 1 11

        Two ->
            Single 2

        Three ->
            Single 3

        Four ->
            Single 4

        Five ->
            Single 5

        Six ->
            Single 6

        Seven ->
            Single 7

        Eight ->
            Single 8

        Nine ->
            Single 9

        Ten ->
            Single 10

        Jack ->
            Single 10

        Queen ->
            Single 10

        King ->
            Single 10


valueToCssClass : Card -> String
valueToCssClass card =
    case card.value of
        Ace ->
            "ace"

        Two ->
            "two"

        Three ->
            "three"

        Four ->
            "four"

        Five ->
            "five"

        Six ->
            "six"

        Seven ->
            "seven"

        Eight ->
            "eight"

        Nine ->
            "nine"

        Ten ->
            "ten"

        Jack ->
            "jack"

        Queen ->
            "queen"

        King ->
            "king"


suiteToCssClass : Card -> String
suiteToCssClass card =
    case card.suite of
        Clubs ->
            "clubs"

        Diamonds ->
            "diamonds"

        Hearts ->
            "hearts"

        Spades ->
            "spades"
