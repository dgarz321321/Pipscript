module Syntax where

{-
PipScript: a small language for writing text-based RPG quests.

Context-Free Grammar: G = (V, T, S, P)

V = {
  <PROGRAM>, <QUEST>, <STATEMENTS>, <STATEMENT>, <SAY>,
  <SPECIAL>, <SET>, <IF>, <CHOICE>, <OPTIONS>, <OPTION>,
  <CONDITION>, <COMPARE>, <EXPRESSION>, <VALUE>,
  <NAME>, <NAME_TAIL>, <NUMBER>, <UNSIGNED>, <DIGIT_TAIL>,
  <STRING>, <TEXT>, <CHAR>, <LETTER>, <DIGIT>
}

T = {
  "QUEST", "SPECIAL", "SAY", "SET", "IF", "ELSE", "CHOICE",
  "{", "}", ";", "=", "+", "-", "->", ">", "<", ">=", "<=", "==",
  '"', "_", " ", ".", ",", "!", "?", "'", ":",
  "a" ... "z", "A" ... "Z", "0" ... "9"
}
The ranges above abbreviate the intervening single-character terminals.
Whitespace between language tokens is not significant; spaces inside
quoted strings are significant. Keywords are treated as reserved tokens.

S = <PROGRAM>

P = {
  <PROGRAM>       -> <QUEST>
  <QUEST>         -> "QUEST" <STRING> "{" <STATEMENTS> "}"
  <STATEMENTS>    -> <STATEMENT> <STATEMENTS> | ε
  <STATEMENT>     -> <SAY> | <SPECIAL> | <SET> | <IF> | <CHOICE>
  <SAY>           -> "SAY" <STRING> ";"
  <SPECIAL>       -> "SPECIAL" <NAME> "=" <NUMBER> ";"
  <SET>           -> "SET" <NAME> "=" <EXPRESSION> ";"
  <IF>            -> "IF" <CONDITION> "{" <STATEMENTS> "}"
                     "ELSE" "{" <STATEMENTS> "}"
  <CHOICE>        -> "CHOICE" "{" <OPTIONS> "}"
  <OPTIONS>       -> <OPTION> <OPTIONS> | <OPTION>
  <OPTION>        -> <STRING> "->" "{" <STATEMENTS> "}"
  <CONDITION>     -> <EXPRESSION> <COMPARE> <EXPRESSION>
  <COMPARE>       -> ">" | "<" | ">=" | "<=" | "=="
  <EXPRESSION>    -> <VALUE>
                   | <VALUE> "+" <EXPRESSION>
                   | <VALUE> "-" <EXPRESSION>
  <VALUE>         -> <NAME> | <NUMBER>
  <NAME>          -> <LETTER> <NAME_TAIL>
  <NAME_TAIL>     -> <LETTER> <NAME_TAIL>
                   | <DIGIT> <NAME_TAIL>
                   | "_" <NAME_TAIL>
                   | ε
  <NUMBER>        -> <UNSIGNED> | "-" <UNSIGNED>
  <UNSIGNED>      -> <DIGIT> <DIGIT_TAIL>
  <DIGIT_TAIL>    -> <DIGIT> <DIGIT_TAIL> | ε
  <STRING>        -> '"' <TEXT> '"'
  <TEXT>          -> <CHAR> <TEXT> | ε
  <CHAR>          -> <LETTER> | <DIGIT> | " " | "." | ","
                   | "!" | "?" | "'" | ":" | "-"
  <LETTER>        -> "a" | "b" | "c" | "d" | "e" | "f"
                   | "g" | "h" | "i" | "j" | "k" | "l"
                   | "m" | "n" | "o" | "p" | "q" | "r"
                   | "s" | "t" | "u" | "v" | "w" | "x"
                   | "y" | "z"
                   | "A" | "B" | "C" | "D" | "E" | "F"
                   | "G" | "H" | "I" | "J" | "K" | "L"
                   | "M" | "N" | "O" | "P" | "Q" | "R"
                   | "S" | "T" | "U" | "V" | "W" | "X"
                   | "Y" | "Z"
  <DIGIT>         -> "0" | "1" | "2" | "3" | "4"
                   | "5" | "6" | "7" | "8" | "9"
}

<NUMBER> denotes a written integer whose value is in ℤ.
ℤ = { ..., -2, -1, 0, 1, 2, ... }. ℤ describes numeric values;
the <NUMBER> and <DIGIT> rules describe the actual syntax.

Each nonterminal above is represented by a matching AST construct or
by a built-in Haskell value (for example, <STRING> is String).
-}

-- Abstract Syntax Tree (AST)
-- A program has one named quest containing zero or more statements.
data Program = Quest String [Stmt]

data Stmt
    = Say String
    | Special String Integer
    | Set String Expr
    | IfElse Cond [Stmt] [Stmt]
    | Choice [Option]

data Option = Option String [Stmt]

data Cond = Compare Expr Comparison Expr

data Comparison
    = GreaterThan
    | LessThan
    | GreaterEqual
    | LessEqual
    | EqualTo

-- This mirrors <EXPRESSION>: VALUE, VALUE + EXPRESSION, VALUE - EXPRESSION.
data Expr
    = Single Value
    | Plus Value Expr
    | Minus Value Expr

data Value
    = Variable String
    | Number Integer

-- Custom Show instances print PipScript syntax, not constructor names.
instance Show Program where
    show (Quest name statements) =
        "QUEST " ++ show name ++ " {\n"
        ++ printStatements 1 statements
        ++ "}"

instance Show Stmt where
    show (Say message) = "SAY " ++ show message ++ ";"
    show (Special name n) = "SPECIAL " ++ name ++ " = " ++ show n ++ ";"
    show (Set name expression) = "SET " ++ name ++ " = " ++ show expression ++ ";"
    show (IfElse cond yesStmts noStmts) =
        "IF " ++ show cond ++ " {\n"
        ++ printStatements 1 yesStmts
        ++ "} ELSE {\n"
        ++ printStatements 1 noStmts
        ++ "}"
    show (Choice options) =
        "CHOICE {\n"
        ++ concatMap (\opt -> indentLines 1 (show opt)) options
        ++ "}"

instance Show Option where
    show (Option label statements) =
        show label ++ " -> {\n"
        ++ printStatements 1 statements
        ++ "}"

instance Show Cond where
    show (Compare left comparison right) =
        show left ++ " " ++ show comparison ++ " " ++ show right

instance Show Comparison where
    show GreaterThan = ">"
    show LessThan = "<"
    show GreaterEqual = ">="
    show LessEqual = "<="
    show EqualTo = "=="

instance Show Expr where
    show (Single value) = show value
    show (Plus value expr) = show value ++ " + " ++ show expr
    show (Minus value expr) = show value ++ " - " ++ show expr

instance Show Value where
    show (Variable name) = name
    show (Number n) = show n

-- Indent each line, including nested blocks.
indentLines :: Int -> String -> String
indentLines level source = unlines (map addIndent (lines source))
  where
    addIndent line = replicate (level * 4) ' ' ++ line

printStatements :: Int -> [Stmt] -> String
printStatements level statements =
    concatMap (\stmt -> indentLines level (show stmt)) statements

printProgram :: Program -> IO ()
printProgram program = putStrLn (show program)
