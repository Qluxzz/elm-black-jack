module Main exposing (main)

import Browser
import Card exposing (Card, Suit(..), Value(..))
import Deck
import Dict
import Hand
import Html
import Html.Attributes
import Html.Events
import Random
import Random.List



{- Black Jack

   Game loop:

   All players in the game has to at least bet the minimum of the table
   The players are dealed two cards, the dealer is dealed two cards, but one is hidden

   Each player is given the choice to take another card or stand,
   When all players are standing or busted, the dealer flips the hidden card.

   If the dealers total sum is below the table minimum (often 16), the dealer takes cards until he reaches 16 or is bust.

   If the dealer busts, all players not busted win.
-}


type alias PlayerId =
    Int


type alias Dollars =
    Int


type Msg
    = ShuffleDeck
    | ShuffledDeck Deck.Deck
      -- Betting
    | Bet Dollars
      -- Deal
    | Deal
      -- Hit or Stand
    | TakeCard
    | Stand
    | Split
    | DoubleDown


type alias Dealer =
    List Card


type alias Model =
    { state : GameState
    , deck : Deck.Deck
    , dealer : Dealer
    , players : Dict.Dict PlayerId Player
    , currentPlayer : PlayerId
    }


type alias Player =
    { hands : List (List Card)
    , money : Int
    , currentBet : Int
    , type_ : PlayerType
    }


type PlayerType
    = Real
    | AI -- TODO: Strategies/Personalities


type GameState
    = ShuffleCards -- Only done at start and if not enough cards left for a new round, TODO: Implement cutting the deck?
    | Betting
    | Dealing
    | HitOrStand
    | Result


init : ( Model, Cmd Msg )
init =
    ( { deck = []
      , dealer = []
      , players =
            Dict.fromList
                [ ( 0
                  , { type_ = Real
                    , money = 5
                    , currentBet = 0
                    , hands = []
                    }
                  )
                ]
      , currentPlayer = 0
      , state = ShuffleCards
      }
    , shuffleDeck (Deck.decks 4)
    )


shuffleDeck : Deck.Deck -> Cmd Msg
shuffleDeck deck =
    Random.generate ShuffledDeck (Random.List.shuffle deck)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = \model -> view model
        , update = update
        , subscriptions = \_ -> Sub.none
        }


deal : Deck.Deck -> List Player -> ( Deck.Deck, Dealer, List Player )
deal deck players =
    let
        ( updatedDeck, updatedPlayers ) =
            List.foldr
                (\player ->
                    \( d, p ) ->
                        let
                            ( cards, d2 ) =
                                Deck.takeCards d 2

                            updatedPlayer : Player
                            updatedPlayer =
                                { player | hands = [ cards ] }
                        in
                        ( d2, updatedPlayer :: p )
                )
                ( deck, [] )
                players

        ( updatedDeck2, updatedDealer ) =
            let
                ( cards, d ) =
                    Deck.takeCards updatedDeck 2
            in
            ( d, cards )
    in
    ( updatedDeck2, updatedDealer, updatedPlayers )


updatePlayer_ :
    Dict.Dict PlayerId Player
    -> PlayerId
    -> (Player -> Player)
    -> Dict.Dict PlayerId Player
updatePlayer_ players index new =
    Dict.update
        index
        (\player -> player |> Maybe.map new)
        players


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatePlayer =
            updatePlayer_ model.players model.currentPlayer

        changeState =
            model.currentPlayer + 1 == Dict.size model.players

        nextPlayer =
            if changeState then
                0

            else
                model.currentPlayer + 1
    in
    case msg of
        ShuffleDeck ->
            ( model, Cmd.none )

        ShuffledDeck deck ->
            ( { model | deck = deck, state = Betting }, Cmd.none )

        {- Betting -}
        -- Need to know which player
        -- Remove bet amount from player money
        -- Set amount as bet
        -- Continue to next player
        Bet amount ->
            ( { model
                | state =
                    if changeState then
                        Dealing

                    else
                        model.state
                , players = updatePlayer (\p -> { p | currentBet = amount, money = p.money - amount })
                , currentPlayer = nextPlayer
              }
            , Cmd.none
            )

        {- DEALING -}
        Deal ->
            let
                ( cards, deck ) =
                    Deck.takeCards model.deck 2
            in
            ( { model
                | deck = deck
                , players = updatePlayer (\p -> { p | hands = [ cards ] })
                , currentPlayer = nextPlayer
                , state =
                    if changeState then
                        HitOrStand

                    else
                        model.state
              }
            , Cmd.none
            )

        {- Hit or Stand -}
        -- Add card to player hand
        -- Need to know which hand, if player has multiple
        TakeCard ->
            let
                ( card, deck ) =
                    Deck.takeCards model.deck 1

                updatedPlayers =
                    updatePlayer
                        (\p ->
                            { p
                                | hands =
                                    p.hands
                                        |> List.head
                                        |> Maybe.map
                                            (\hand ->
                                                -- TODO: Inform player that they have bust
                                                if Tuple.first (Hand.value (hand ++ card)) > 21 then
                                                    []

                                                else
                                                    [ hand ++ card ]
                                            )
                                        |> Maybe.withDefault [ card ]
                            }
                        )
            in
            ( { model | deck = deck, players = updatedPlayers }, Cmd.none )

        -- Continue to next player
        -- Stand on which hand?
        Stand ->
            ( { model
                | currentPlayer = nextPlayer
                , state =
                    if changeState then
                        Result

                    else
                        model.state
              }
            , Cmd.none
            )

        -- Split current player hands into two, allow player to take a card/stand/split/doubledown on each hand
        Split ->
            let
                updatedPlayers =
                    updatePlayer
                        (\p ->
                            { p
                                | hands =
                                    case p.hands of
                                        first :: _ ->
                                            case first of
                                                first_card :: second_card :: _ ->
                                                    [ [ first_card ], [ second_card ] ]

                                                [ _ ] ->
                                                    []

                                                [] ->
                                                    []

                                        [] ->
                                            []
                            }
                        )
            in
            ( { model | players = updatedPlayers }, Cmd.none )

        -- Double bet and take another card
        DoubleDown ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ dealerView model.dealer
        , Html.div [ Html.Attributes.style "display" "flex" ]
            (List.map (playerView model.state) (Dict.values model.players))
        ]


dealerView : Dealer -> Html.Html Msg
dealerView dealer =
    -- TODO: While we're not in the results state, the second card of the dealer should be hidden
    Html.div [] (List.map cardView dealer ++ [ Html.p [] [ Html.text (handValue dealer) ] ])


playerView : GameState -> Player -> Html.Html Msg
playerView state player =
    Html.div []
        [ Html.p []
            [ Html.text ("$" ++ String.fromInt player.money) ]
        , case state of
            Betting ->
                Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
                    (List.map
                        (\amount ->
                            Html.button [ Html.Events.onClick (Bet amount), Html.Attributes.disabled (player.money < amount) ] [ Html.text ("Bet $" ++ String.fromInt amount) ]
                        )
                        [ 1, 5, 10, 25, 50 ]
                    )

            Dealing ->
                Html.div []
                    [ Html.button [ Html.Events.onClick Deal ] [ Html.text "Deal!" ]
                    ]

            HitOrStand ->
                Html.div []
                    (List.map
                        handView
                        player.hands
                    )

            _ ->
                Html.text ""
        ]


canSplit : Hand.Hand -> Bool
canSplit hand =
    case hand of
        first :: second :: _ ->
            first.value == second.value

        _ ->
            False


handValue : Hand.Hand -> String
handValue hand =
    hand
        |> Hand.value
        |> (\( v1, v2 ) ->
                if v1 == v2 then
                    String.fromInt v1

                else
                    String.fromInt v1 ++ "/" ++ String.fromInt v2
           )


handView : Hand.Hand -> Html.Html Msg
handView hand =
    Html.div []
        (List.map cardView hand
            ++ [ Html.p [] [ Html.text (handValue hand) ]
               , Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "10px" ]
                    [ Html.button [ Html.Events.onClick TakeCard ] [ Html.text "Take a card" ]
                    , Html.button [ Html.Events.onClick Stand ] [ Html.text "Stand" ]
                    , if canSplit hand then
                        Html.button [ Html.Events.onClick Split ] [ Html.text "Split" ]

                      else
                        Html.text ""
                    , if List.length hand == 2 then
                        Html.button [ Html.Events.onClick DoubleDown ] [ Html.text "Double down" ]

                      else
                        Html.text ""
                    ]
               ]
        )


cardView : Card -> Html.Html msg
cardView card =
    let
        symbol : Card -> Html.Html msg
        symbol { suite } =
            let
                black s =
                    Html.span [] [ Html.text s ]

                red s =
                    Html.span [ Html.Attributes.style "color" "red" ] [ Html.text s ]
            in
            case suite of
                Clubs ->
                    black "♣"

                Diamonds ->
                    red "♦"

                Hearts ->
                    red "♥"

                Spades ->
                    black "♠"

        value : Card -> String
        value c =
            case c.value of
                Ace ->
                    "A"

                Two ->
                    "2"

                Three ->
                    "3"

                Four ->
                    "4"

                Five ->
                    "5"

                Six ->
                    "6"

                Seven ->
                    "7"

                Eight ->
                    "8"

                Nine ->
                    "9"

                Ten ->
                    "10"

                Jack ->
                    "J"

                Queen ->
                    "Q"

                King ->
                    "K"
    in
    Html.div [] [ symbol card, Html.span [] [ Html.text (value card) ] ]
