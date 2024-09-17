import Data.List
import System.IO
import Data.Char
import Data.Maybe

-- EXPRESSION DATA------------------------------------------------------------------------------------------------

data Expression = Val Int
                | Bin Char Expression Expression 
                | Unar Char Expression
                | Hist Int
                deriving Show


-- PARSING THROUGH ------------------------------------------------------------------------------------------------

parseExp :: String -> Maybe (Expression, String)
parseExp [] = Nothing
parseExp (x:xs)
  | isDigit x = parseDig (x:xs)
  | x == '$'  = parseHist xs
  | x `elem` "+-*/" = parseOp x xs
  | isSpace x = parseExp xs
  | otherwise = Nothing 

parseDig :: String -> Maybe (Expression, String)
parseDig xs =
  let (digs, rest) = span isDigit xs
  in Just (Val (read digs), rest)

parseHist :: String -> Maybe (Expression, String)
parseHist cs =
  let (digs, rest) = span isDigit cs
  in if null digs then Nothing else Just (Hist (read digs), rest)
     
parseOp :: Char -> String -> Maybe (Expression, String)
parseOp op cs = 
    do
  (expr1, next) <- parseExp cs
  (expr2, remaining) <- parseExp next
  case op of
    '+' -> Just (Bin '+' expr1 expr2, remaining)
    '-' -> Just (Bin '-' expr1 expr2, remaining)
    '*' -> Just (Bin '*' expr1 expr2, remaining)
    '/' -> Just (Bin '/' expr1 expr2, remaining)
    _   -> Nothing

-- EVAL EXPRESSION ------------------------------------------------------------------------------------------------------------------------

evaluate :: Expression -> [Int] -> Maybe (Int, [Int])
evaluate (Val n) _ = Just (n, [])
evaluate (Bin op expr1 expr2) history = do
  (val1, remaining1) <- evaluate expr1 history
  (val2, remaining2) <- evaluate expr2 history
  let result = case op of
                 '+' -> val1 + val2
                 '-' -> val1 - val2
                 '*' -> val1 * val2
                 '/' -> val1 `div` val2 -- if val2 == 0 then Nothing else Just 
                -- _   -> Nothing  -- Invalid operator
  case result of
    res -> Just (res, remaining1 ++ remaining2)
evaluate (Unar '-' expr) history = do
  (val, remaining) <- evaluate expr history
  Just (-val, remaining)
evaluate (Hist id) history =
  case history !? id of
    Just val -> Just (val, history)
    Nothing  -> Nothing


-- PRINTING --------------------------------------------------------------------------------------------------------------------------------
printHist :: [Int] -> IO ()
printHist hist =
  putStrLn "History:" >> mapM_ printHistElem (zip [1..] (reverse hist))
  where
    printHistElem (index, value) = putStrLn $ "ID " ++ show index ++ ": " ++ show value

-- MORE HELPERS ------------------------------------------------------------------------------------------------------------------------

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)


-- UPDATING HIST

updHist :: String -> [Int] -> Maybe (Int, [Int])
updHist expr history =
  case parseExp expr of
    Just (parsedExpr, "") -> do
      (result, _) <- evaluate parsedExpr history  
      let updatedHistory = result : history  
      Just (result, updatedHistory)
    _ -> Nothing


-- MAIN ------------------------------------------------------------------------------------------------------------------------------------------------
main = do
    putStrLn "Input your prefix expression!"
    evalLoop []


-- EVALUATION LOOP ------------------------------------------------------------------------------------------------------------------------------------------
evalLoop :: [Int] -> IO ()
evalLoop h = do
  input <- getLine
  case updHist input h of
    Just (result, updatHist) -> do
      putStrLn $ "Result: " ++ show result
      printHist updatHist
      evalLoop updatHist
    Nothing -> do
      putStrLn "Invalid Expression"
      evalLoop h