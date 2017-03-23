import Data.Char
import Data.Maybe
import System.Environment
import Text.Read
import Auto

newtype Alpha = Alpha Char deriving (Eq, Show)

alphaToChar :: Alpha -> Char
alphaToChar (Alpha i) = i

instance Bounded Alpha where
  minBound = Alpha 'A'
  maxBound = Alpha 'Z'

instance Enum Alpha where
  toEnum = Alpha . chr
  fromEnum = ord . alphaToChar

instance Read Alpha where
  readsPrec _ (char:string) = [(Alpha char, string)]
  readsPrec _ _ = []

getItem :: [a] -> Int -> Maybe a
getItem list n | n >= length list = Nothing
               | otherwise = Just $ list!!n

parseStatesList :: String -> Int -> [Int]
parseStatesList s upperLimit = case readMaybe s of
                             Just x -> if all (\x -> x>0 && x<=upperLimit) x
                               then x
                               else error "BAD INPUT - using nonexistent states."
                             _ -> error "BAD INPUT - parse error in second or third line."

parseStatesQuota :: String -> Int
parseStatesQuota s = case readMaybe s of
                       Just x -> if x < 0
                         then error "BAD INPUT - can't have automat with less than 0 states."
                         else x
                       _ -> error "BAD INPUT - parse error in first line."

maybeReadAlphaList :: String -> Maybe [Alpha]
maybeReadAlphaList s = let mapped = map (\x -> readMaybe [x]) s in
                         if any (isNothing) mapped
                         then Nothing
                         else Just $ map fromJust mapped

parseTransition :: [String] -> [(Int, Alpha, [Int])]
parseTransition s | length s < 2 = error "BAD INPUT - too few arguments in transition definition."
                  | otherwise = let
                      (from:by:to) = s
                      fromState = (readMaybe from) :: Maybe Int
                      letters = maybeReadAlphaList by
                      toStates = (map readMaybe to) :: [Maybe Int]
                    in
                      if any (isNothing) (letters, fromState) || any (isNothing) toStates
                      then error "BAD INPUT - parse error in transition definition."
                      else let
                        resFrom = fromJust fromState
                        resTo = map fromJust toStates
                      in foldr (\char -> ((resFrom, char, resTo):)) [] (fromJust letters)
  
parseInputFile :: [String] -> (Int, [Int], [Int], [(Int, Alpha, [Int])])
parseInputFile s | length s < 3 = error $ "BAD INPUT - too few lines in provided file."++(show $ length s)
                 | otherwise = let
                     (statesNum:initStates:terminalStates:transitions) = s
                     n = parseStatesQuota $ statesNum
                   in (n, parseStatesList initStates n, parseStatesList terminalStates n,
                      foldr (\line -> ((parseTransition $ words line)++)) [] transitions)

parseWord :: String -> [Alpha]
parseWord s = case maybeReadAlphaList s of
                Just x -> x
                Nothing -> error "BAD INPUT - incorrect word inputted"
  
main = do
  [fileName] <- getArgs
  inputFile <- readFile fileName
  let list = filter (not . null) $ lines inputFile
  let (a,b,c,d) = parseInputFile $ init list
  let automat = fromLists [1..a] b c d
  let word = parseWord $ last list
  print $ accepts automat word
