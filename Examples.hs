module Main where

import Syntax

{-
PipScript Example Program 1 (Raider Encounter):

QUEST "Raider Encounter" {
    SPECIAL health = 100;
    SPECIAL strength = 8;
    SAY "A raider approaches!";
    IF strength >= 7 {
        SAY "You defeated the raider!";
    } ELSE {
        SAY "The raider attacks!";
        SET health = health - 20;
    }
    CHOICE {
        "Search" -> {
            SAY "You found 25 caps!";
        }
        "Leave" -> {
            SAY "You walk away.";
        }
    }
}
-}

p1 :: Program
p1 = Quest "Raider Encounter"
    [ Special "health" 100
    , Special "strength" 8
    , Say "A raider approaches!"
    , IfElse (Compare
                 (Single (Variable "strength"))
                 GreaterEqual
                 (Single (Number 7)))
        [ Say "You defeated the raider!" ]
        [ Say "The raider attacks!"
        , Set "health" (Minus (Variable "health") (Single (Number 20)))
        ]
    , Choice
        [ Option "Search" [Say "You found 25 caps!"]
        , Option "Leave" [Say "You walk away."]
        ]
    ]

{-
PipScript Example Program 2 (Diamond City Market):

QUEST "Diamond City Market" {
    SPECIAL caps = 100;
    SAY "Welcome to the market!";
    CHOICE {
        "Buy Stimpak" -> {
            IF caps >= 50 {
                SET caps = caps - 50;
                SAY "You bought a Stimpak!";
            } ELSE {
                SAY "Not enough caps!";
            }
        }
        "Sell Junk" -> {
            SET caps = caps + 15;
            SAY "You earned 15 caps!";
        }
    }
}
-}

p2 :: Program
p2 = Quest "Diamond City Market"
    [ Special "caps" 100
    , Say "Welcome to the market!"
    , Choice
        [ Option "Buy Stimpak"
            [ IfElse (Compare
                         (Single (Variable "caps"))
                         GreaterEqual
                         (Single (Number 50)))
                [ Set "caps" (Minus (Variable "caps") (Single (Number 50)))
                , Say "You bought a Stimpak!"
                ]
                [ Say "Not enough caps!" ]
            ]
        , Option "Sell Junk"
            [ Set "caps" (Plus (Variable "caps") (Single (Number 15)))
            , Say "You earned 15 caps!"
            ]
        ]
    ]

main :: IO ()
main = do
    putStrLn "First program:"
    printProgram p1
    putStrLn ""
    putStrLn "Second program:"
    printProgram p2
