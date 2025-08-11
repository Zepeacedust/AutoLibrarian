-- |

module Calculator where

import Text.Parsec.Prim
import Text.Parsec


import qualified Data.List as L

import Data.Ratio
import Control.Arrow (Arrow(second))

data Token = NumTok Rational | UnitTok String | OperatorTok Operator deriving (Show, Eq)

data Operator = Add | Sub | Mul | Div | Pow deriving (Show, Eq)

data AST = BinOp AST Operator AST | UnaryOp Operator AST | Numb Rational | Unit String | Func String AST deriving Show

data Value = Value Rational TypeSignature deriving Show

type TypeSignature = [(String, Rational)]

data CalcCommand = Simple AST | Formatted AST String deriving Show

fromEither :: Either a a -> a
fromEither (Right m) = m
fromEither (Left m) = m

parseCalc :: String -> String
parseCalc m = fromEither $ do
  let parsed = parse parseCommand "" m
  command <- case parsed of
    Right comm -> return comm
    Left _ -> Left "Parse Error"
  case command of
    Simple ast -> prettyEval ast
    Formatted ast format -> formatEval ast format

parseCommand :: Parsec String () CalcCommand
parseCommand = do
  _ <-string' "!calc"
  lhs <- parseEquation
  (do
       _ <- spaceWrapped $ string' "to"
       rhs <- many anyChar
       return $ Formatted lhs rhs
   ) <|> return ( Simple lhs)

prettyEval :: AST -> Either String String
prettyEval input = do
  Value inputV inputT <- evaluate defaultUnits input
  return $ prettyNumber inputV ++ " " ++  prettyTypes inputT

prettyCalc :: String -> Either String String
prettyCalc input = do
  Value inputV inputT <- calculate input
  return $ prettyNumber inputV ++ " " ++  prettyTypes inputT

formatEval :: AST -> String -> Either String String
formatEval input format = do
  Value inputV inputT <- evaluate defaultUnits input
  Value formatV formatT <- calculate format
  if formatT /= inputT then return "Incompatible input and format types"
    else
      return $ prettyNumber (inputV/formatV) ++ " " ++ format


formatCalc :: String -> String -> Either String String
formatCalc input format  = do
  Value inputV inputT <- calculate input
  Value formatV formatT <- calculate format
  if formatT /= inputT then return "Incompatible input and format types"
    else
      return $ prettyNumber (inputV/formatV) ++ " " ++ format


scale :: Value -> Value -> String
scale (Value form _formT) (Value n _types) =
  show (n/form)

prettyNumber :: Rational -> String
prettyNumber = show.fromRational

prettyType :: (String, Rational) -> String
prettyType (name, amount)
  | amount == 1 = name
  | otherwise   = name ++ "^" ++ prettyNumber amount


prettyTypes :: TypeSignature -> String
prettyTypes t = if unders == "" then overs else overs ++ "/" ++ unders where
  overDims = filter (\l -> snd l > 0) t

  underDims = filter (\l -> snd l < 0) t

  overs = L.intercalate "*" (map prettyType overDims)
  unders = L.intercalate "*" (map prettyType underDims)

calculate :: String -> Either String Value
calculate s = do
  let parsed = parse parseEquation "" s
  case parsed of
    Right expression -> evaluate defaultUnits expression
    Left _ -> Left "Parse Error"



evaluate :: [(String, Value)] -> AST -> Either String Value

evaluate _env (Numb x) = Right (Value x [])

evaluate env (Unit s) = case lookup s env of
  Nothing -> Right (Value 1 [(s,1)])
  Just val -> Right val

evaluate env (UnaryOp _operator inner) = do
  Value innerVal innerType <- evaluate env inner
  return $ Value (-innerVal) innerType

evaluate env (BinOp lhs op rhs) = do
  right <- evaluate env rhs
  left  <- evaluate env lhs
  case op of
    Add -> valAdd left right
    Sub -> valSub left right
    Mul -> valMul left right
    Div -> valDiv left right
    Pow -> valPow left right

evaluate env (Func name op) = do
  operand <- evaluate env op
  case lookup name functions of
    Nothing -> Left "Function does not exist"
    Just func -> func operand

functions :: [(String, Value -> Either String Value)]
functions = [
  ("circleRadius", \a -> do
      inter <- valDiv a (Value (toRational pi) [])
      valPow inter (Value 0.5 [])
  ),
  ("sphereRadius", \v-> do
      inter <- valDiv v (Value (4/3 * toRational pi) [])
      valPow inter (Value (1/3) [])
  )
  ]


valAdd :: Value -> Value -> Either String Value
valAdd (Value lhv lht) (Value rhv rht) = do
  if rht == lht
    then return (Value (lhv + rhv) rht)
    else Left ("Incomparable types: " ++ prettyTypes rht ++ " and " ++ prettyTypes lht)

valSub :: Value -> Value -> Either String Value
valSub (Value lhv lht) (Value rhv rht) = do
  if rht == lht
    then return (Value (lhv - rhv) rht)
    else Left ("Incomparable types: " ++ prettyTypes rht ++ " and " ++ prettyTypes lht)

valMul :: Value -> Value -> Either String Value
valMul (Value lhv lht) (Value rhv rht) = do
  return (Value (lhv * rhv) (typeAdd lht rht))

valDiv :: Value -> Value -> Either String Value
valDiv (Value lhv lht) (Value rhv rht) = do
  if rhv == 0 then Left "Division by zero"
    else return (Value (lhv / rhv) (typeSub lht rht))

valPow :: Value -> Value -> Either String Value
valPow (Value lhv lht) (Value rhv rht) = do
  if null rht
    then return (Value (toRational ((fromRational lhv :: Double) ** (fromRational rhv :: Double))) (typeMul lht rhv))
    else Left "Exponents must be dimensionless"


-- pínu silly time complexity, en nennis
-- ætti líka bara að vera að reikna frekar einfalda hluti
insertMerging :: Ord a => b -> (b -> b -> b) -> [(a,b)] -> (a,b) -> [(a,b)]
insertMerging def merger [] (newKey, newVal) = [(newKey, merger def newVal)]
insertMerging def merger ((oldKey, oldVal):xs) (newKey, newVal) = case compare newKey oldKey of
  LT -> (oldKey, oldVal) : insertMerging def merger xs (newKey, newVal) -- Keep going
  EQ -> (oldKey, merger oldVal newVal) : xs                             -- Hit
  GT -> (newKey, merger def    newVal) : (oldKey, oldVal) : xs          -- Overshot

typeAdd :: TypeSignature -> TypeSignature -> TypeSignature
typeAdd lhs rhs = filter (\l -> snd l /= 0) (L.foldl' (insertMerging 0 (+)) lhs rhs)

typeSub :: TypeSignature -> TypeSignature -> TypeSignature
typeSub lhs rhs = filter (\l -> snd l /= 0) (L.foldl' (insertMerging 0 (-)) lhs rhs)

typeMul :: TypeSignature -> Rational -> TypeSignature
typeMul typeSig n= map (\(key, val) -> (key, val * n)) typeSig


spaceWrapped :: Parsec String () a -> Parsec String () a
spaceWrapped inner = do
  spaces
  out <- inner
  spaces
  return out

parseOp :: Operator -> Parsec String () Operator
parseOp Add = spaceWrapped $ do
  _<-char '+'
  return Add
parseOp Sub = spaceWrapped $ do
  _<-char '-'
  return Sub
parseOp Mul = spaceWrapped $ do
  _<-char '*'
  return Mul
parseOp Div = spaceWrapped $ do
  _<-char '/'
  return Div
parseOp Pow = spaceWrapped $ do
  _<-char '^'
  return Pow

stackup :: AST -> (Operator, AST) -> AST
stackup lhs (op, rhs) = BinOp lhs op rhs

parseEquation :: Parsec String () AST
parseEquation = spaceWrapped $ do
  parseAdditive

parseAdditive :: Parsec String () AST
parseAdditive = spaceWrapped $ do
  first <- parseMultiplicative
  adds <- many (do
                     operator <- parseOp Add <|> parseOp Sub
                     out <- parseMultiplicative
                     return (operator, out)
                 )

  return $ L.foldl' stackup first adds

parseMultiplicative :: Parsec String () AST
parseMultiplicative = spaceWrapped $ do
  first <- parsePow
  muls <- many (do
                     operator <- parseOp Mul <|> parseOp Div
                     out <- parsePow
                     return (operator, out)
                 )

  return $ L.foldl' stackup first muls

parsePow :: Parsec String () AST
parsePow = spaceWrapped $ do
  first <- parseUnary
  powers <- many (do
                     operator <- parseOp Pow
                     out <- parseUnary
                     return (operator, out)
                 )

  return $ L.foldl' stackup first powers

parseUnit :: Parsec String () AST
parseUnit = spaceWrapped $ do
  lead <- letter
  rest <- many alphaNum
  (do
      _ <- char '('
      inner <- parseEquation
      _ <- char ')'
      return $ Func (lead:rest) inner
      )<|> return (Unit (lead:rest))

parseNumber :: Parsec String () AST
parseNumber = spaceWrapped $ do
  num <- sepBy1 (many digit) (char '.')
  case num of
    [""]    -> parserFail "Improperly formatted number"
    [n]     -> return $ Numb $ read n % 1
    ["", d] -> return $ Numb $ read d % 10 ^ length d
    [n, d]  -> return $ Numb $ (read n * 10 ^ length d + read d) % 10 ^ length d
    _fail   -> parserFail "Improperly formatted number"

parseParens :: Parsec String () AST
parseParens = spaceWrapped $ do
  _ <- char '('
  out <- parseEquation
  _ <- char ')'
  return out

parseUnarySub :: Parsec String () AST
parseUnarySub = spaceWrapped $ do
  _ <- char '-'
  UnaryOp Sub <$> parseEquation

parseUnary :: Parsec String () AST
parseUnary = parseParens <|> parseUnit <|> parseNumber <|> parseUnarySub

defaultUnits :: [(String, Value)]
defaultUnits = [
  ("sec",      Value 1           [("sec",   1)]),
  ("Sec",      Value 1           [("sec",   1)]),
  ("second",      Value 1           [("sec",   1)]),
  ("Second",      Value 1           [("sec",   1)]),
  ("seconds",      Value 1           [("sec",   1)]),
  ("Seconds",      Value 1           [("sec",   1)]),
  ("s",        Value 1           [("sec",   1)]),
  ("m",      Value 60          [("sec",   1)]),
  ("min",      Value 60          [("sec",   1)]),
  ("Min",      Value 60          [("sec",   1)]),
  ("minute",      Value 60          [("sec",   1)]),
  ("minute",      Value 60          [("sec",   1)]),
  ("h",     Value 3600        [("sec",   1)]),
  ("hour",     Value 3600        [("sec",   1)]),
  ("Hour",     Value 3600        [("sec",   1)]),
  ("hours",     Value 3600        [("sec",   1)]),
  ("Hours",     Value 3600       [("sec",   1)]),
  ("day",      Value 86400       [("sec",   1)]),
  ("Day",      Value 86400       [("sec",   1)]),
  ("days",      Value 86400      [("sec",   1)]),
  ("Days",      Value 86400      [("sec",   1)]),
  ("week",     Value 604800      [("sec",   1)]),
  ("Week",     Value 604800      [("sec",   1)]),
  ("weeks",     Value 604800     [("sec",   1)]),
  ("Weeks",     Value 604800     [("sec",   1)]),
  ("month",    Value 2629743.83  [("sec",   1)]),
  ("Month",    Value 2629743.83 [("sec",   1)]),
  ("months",    Value 2629743.83 [("sec",   1)]),
  ("Months",    Value 2629743.83 [("sec",   1)]),
  ("season",   Value 7889231.49  [("sec",   1)]),
  ("Season",   Value 7889231.49  [("sec",   1)]),
  ("seasons",   Value 7889231.49  [("sec",   1)]),
  ("Seasons",   Value 7889231.49 [("sec",   1)]),
  ("year",     Value 31556926    [("sec",   1)]),
  ("years",     Value 31556926   [("sec",   1)]),
  ("cm",       Value 1          [("cm",    1)]),
  ("centimeter",       Value 1          [("cm",    1)]),
  ("centimeters",       Value 1          [("cm",    1)]),
  ("Centimeter",       Value 1          [("cm",    1)]),
  ("Centimeters",       Value 1          [("cm",    1)]),
  ("centimetre",       Value 1          [("cm",    1)]),
  ("Centimetre",       Value 1          [("cm",    1)]),
  ("centimetres",       Value 1          [("cm",    1)]),
  ("Centimetres",       Value 1          [("cm",    1)]),
  ("m",        Value 100        [("cm",    1)]),
  ("meter",        Value 100    [("cm",    1)]),
  ("Meter",        Value 100    [("cm",    1)]),
  ("meters",        Value 100   [("cm",    1)]),
  ("Meters",        Value 100   [("cm",    1)]),
  ("metre",        Value 100   [("cm",    1)]),
  ("Metre",        Value 100   [("cm",    1)]),
  ("metres",        Value 100   [("cm",    1)]),
  ("Metres",        Value 100   [("cm",    1)]),
  ("km",       Value 100000     [("cm",    1)]),
  ("kilometer",       Value 100000     [("cm",    1)]),
  ("kilometers",       Value 100000     [("cm",    1)]),
  ("Kilometer",       Value 100000     [("cm",    1)]),
  ("Kilometers",       Value 100000     [("cm",    1)]),
  ("kilometre",       Value 100000     [("cm",    1)]),
  ("kilometres",       Value 100000     [("cm",    1)]),
  ("Kilometre",       Value 100000     [("cm",    1)]),
  ("Kilometres",       Value 100000     [("cm",    1)]),
  ("inch",     Value 2.54       [("cm",    1)]),
  ("Inch",     Value 2.54       [("cm",    1)]),
  ("inches",     Value 2.54     [("cm",    1)]),
  ("Inches",     Value 2.54     [("cm",    1)]),
  ("ft",     Value 30.48      [("cm",    1)]),
  ("foot",     Value 30.48      [("cm",    1)]),
  ("Foot",     Value 30.48      [("cm",    1)]),
  ("feet",     Value 30.48      [("cm",    1)]),
  ("Feet",     Value 30.48      [("cm",    1)]),
  ("yard",     Value 91.44      [("cm",    1)]),
  ("Yard",     Value 91.44      [("cm",    1)]),
  ("yards",     Value 91.44     [("cm",    1)]),
  ("Yards",     Value 91.44     [("cm",    1)]),
  ("pace",     Value 91.44      [("cm",    1)]),
  ("paces",     Value 91.44     [("cm",    1)]),
  ("mile",     Value 160934.4   [("cm",    1)]),
  ("Mile",     Value 160934.4   [("cm",    1)]),
  ("miles",     Value 160934.4  [("cm",    1)]),
  ("Miles",     Value 160934.4  [("cm",    1)]),
  ("denarius", Value 1          [("penny", 1)]),
  ("Denarius", Value 1          [("penny", 1)]),
  ("denarii", Value 1          [("penny", 1)]),
  ("Denarii", Value 1          [("penny", 1)]),
  ("penny", Value 1          [("penny", 1)]),
  ("Penny", Value 1          [("penny", 1)]),
  ("pennies", Value 1          [("penny", 1)]),
  ("Pennies", Value 1          [("penny", 1)]),
  ("solidus",  Value 12         [("penny", 1)]),
  ("Solidus",  Value 12         [("penny", 1)]),
  ("solidi",  Value 12         [("penny", 1)]),
  ("Solidi",  Value 12         [("penny", 1)]),
  ("shilling",  Value 12         [("penny", 1)]),
  ("Shilling",  Value 12         [("penny", 1)]),
  ("shillings",  Value 12         [("penny", 1)]),
  ("Shillings",  Value 12         [("penny", 1)]),
  ("libra",    Value 240        [("penny", 1)]),
  ("pi",       Value (toRational pi)         []),
  ("g",        Value 1          [("gram", 1)]),
  ("gram",     Value 1          [("gram", 1)]),
  ("gram",     Value 1          [("gram", 1)]),
  ("grams",     Value 1         [("gram", 1)]),
  ("Grams",     Value 1         [("gram", 1)]),
  ("kg", Value 1000             [("gram", 1)]),
  ("kilogram", Value 1000       [("gram", 1)]),
  ("Kilogram", Value 1000       [("gram", 1)]),
  ("kilograms", Value 1000      [("gram", 1)]),
  ("Kilograms", Value 1000      [("gram", 1)]),
  ("lb", Value 453.59237        [("gram", 1)]),
  ("pound", Value 453.59237     [("gram", 1)]),
  ("Pound", Value 453.59237     [("gram", 1)]),
  ("pounds", Value 453.59237    [("gram", 1)]),
  ("Pounds", Value 453.59237    [("gram", 1)]),
  ("kilo", Value 1000          [("gram", 1)]),
  ("Kilo", Value 1000          [("gram", 1)]),
  ("kilos", Value 1000          [("gram", 1)]),
  ("Kilos", Value 1000          [("gram", 1)]),
  ("ton",      Value 1000000    [("gram", 1)]),
  ("Ton",      Value 1000000    [("gram", 1)]),
  ("tons",      Value 1000000   [("gram", 1)]),
  ("Tons",      Value 1000000   [("gram", 1)]),
  ("pawn",     Value 1          [("pawn", 1)]),
  ("Pawn",     Value 1          [("pawn", 1)]),
  ("pawns",     Value 1         [("pawn", 1)]),
  ("Pawns",     Value 1         [("pawn", 1)]),
  ("rook",     Value 10         [("pawn", 1)]),
  ("Rook",     Value 10         [("pawn", 1)]),
  ("rooks",     Value 10        [("pawn", 1)]),
  ("Rooks",     Value 10        [("pawn", 1)]),
  ("queen",    Value 100        [("pawn", 1)]),
  ("Queen",    Value 100        [("pawn", 1)]),
  ("queens",    Value 100       [("pawn", 1)]),
  ("Queens",    Value 100       [("pawn", 1)]),
  ("liter",    Value 1000       [("cm",   3)]),
  ("Liter",    Value 1000       [("cm",   3)]),
  ("liters",    Value 1000      [("cm",   3)]),
  ("Liters",    Value 1000      [("cm",   3)])
  ]
