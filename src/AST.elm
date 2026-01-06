module AST exposing (Action(..), handAction)

import Parser exposing (..)


type Action
    = Hit
    | Stand
    | Split
    | Double


type Expr
    = If Expr Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | Not Expr
    | Compare Op Value Value
    | BoolLiteral Bool
    | ActionLiteral Action


type Op
    = Lt
    | Lte
    | Gt
    | Gte
    | Eq


type Value
    = Player
    | Dealer
    | CanSplit
    | IntLiteral Int


keyword_ : String -> Parser ()
keyword_ kw =
    keyword kw |. spaces


lexeme : Parser a -> Parser a
lexeme parser =
    parser |. spaces


symbol_ : String -> Parser ()
symbol_ sym =
    symbol sym |. spaces


actionParser : Parser Expr
actionParser =
    oneOf
        [ keyword_ "hit" |> map (\_ -> ActionLiteral Hit)
        , keyword_ "stand" |> map (\_ -> ActionLiteral Stand)
        , keyword_ "split" |> map (\_ -> ActionLiteral Split)
        , keyword_ "double" |> map (\_ -> ActionLiteral Double)
        ]


valueParser : Parser Value
valueParser =
    oneOf
        [ keyword_ "player" |> map (\_ -> Player)
        , keyword_ "dealer" |> map (\_ -> Dealer)
        , keyword_ "canSplit" |> map (\_ -> CanSplit)
        , lexeme int |> map IntLiteral
        ]


comparisonParser : Parser Expr
comparisonParser =
    succeed (\l op r -> Compare op l r)
        |= valueParser
        |= operator
        |= valueParser


operator : Parser Op
operator =
    oneOf
        [ symbol_ "<=" |> map (\_ -> Lte)
        , symbol_ ">=" |> map (\_ -> Gte)
        , symbol_ "<" |> map (\_ -> Lt)
        , symbol_ ">" |> map (\_ -> Gt)
        , symbol_ "==" |> map (\_ -> Eq)
        ]


ifExpr : Parser Expr
ifExpr =
    succeed If
        |. keyword_ "if"
        |= exprParser
        |. keyword_ "then"
        |= exprParser
        |. keyword_ "else"
        |= exprParser


exprParser : Parser Expr
exprParser =
    Parser.lazy (\_ -> exprHelp)


exprHelp : Parser Expr
exprHelp =
    oneOf
        [ ifExpr
        , actionParser
        , comparisonParser
        ]


type alias Env =
    { player : Int
    , dealer : Int
    , canSplit : Bool
    }


eval : Env -> Expr -> Action
eval env expr =
    case expr of
        If cond yes no ->
            if evalBool env cond then
                eval env yes

            else
                eval env no

        ActionLiteral action ->
            action

        _ ->
            Stand


evalBool : Env -> Expr -> Bool
evalBool env expr =
    case expr of
        Compare op l r ->
            evalComparison op (evalValue env l) (evalValue env r)

        _ ->
            False


evalValue : Env -> Value -> Int
evalValue env value =
    case value of
        Player ->
            env.player

        Dealer ->
            env.dealer

        CanSplit ->
            if env.canSplit then
                1

            else
                0

        IntLiteral n ->
            n


evalComparison : Op -> Int -> Int -> Bool
evalComparison op left right =
    case op of
        Lt ->
            left < right

        Lte ->
            left <= right

        Gt ->
            left > right

        Gte ->
            left >= right

        Eq ->
            left == right


handAction : String -> Int -> Int -> Bool -> Result String Action
handAction source player dealer canSplit =
    case Parser.run exprParser source of
        Ok ast ->
            Ok <|
                eval
                    { player = player
                    , dealer = dealer
                    , canSplit = canSplit
                    }
                    ast

        Err err ->
            Err (Debug.toString err)
