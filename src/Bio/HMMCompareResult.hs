-- | Parse HMMCompare output

module Bio.HMMCompareResult
    (
     HMMCompareResult(..),
     parseHMMCompareResult,
     readHMMCompareResult,
     getHMMCompareResults,
     getModelsNames,
     getModelNames
    ) where

import Text.ParserCombinators.Parsec
import Data.List

-- | Datastructure for result strings of comparisons between covariance models by HMMCompare
data HMMCompareResult = HMMCompareResult
  { model1Name :: String,
    model2Name :: String,
    linkscore1 :: Double,
    linkscore2 :: Double,
    linksequence  :: String,
    model1matchednodes :: [Int],
    model2matchednodes :: [Int]
  } deriving ()

instance Show HMMCompareResult where
  show (HMMCompareResult _model1Name _model2Name _linkscore1 _linkscore2 _linksequence  _model1matchednodes _model2matchednodes) =  _model1Name ++ "   " ++ _model2Name ++ "     " ++ show _linkscore1 ++ "     " ++ show _linkscore2 ++ " " ++ _linksequence ++ " " ++ formatMatchedNodes _model1matchednodes ++ " " ++ formatMatchedNodes _model2matchednodes ++ "\n"

formatMatchedNodes :: [Int] -> String
formatMatchedNodes nodes = "[" ++ intercalate "," (map show nodes) ++ "]"

-- | parse HMMCompareResult model from input string
parseHMMCompareResult :: String -> Either ParseError [HMMCompareResult]
parseHMMCompareResult input = parse genParseHMMCompareResults "HMMCompareResult" input

-- | parse HMMCompareResult from input filePath                      
readHMMCompareResult :: String -> IO (Either ParseError [HMMCompareResult])
readHMMCompareResult filePath = do
  parseFromFile genParseHMMCompareResults filePath

-- | Parse the input as HMMCompareResult datatype
genParseHMMCompareResults :: GenParser Char st [HMMCompareResult]
genParseHMMCompareResults = do
  hmmcs  <- many1 (try genParseHMMCompareResult)
  eof
  return hmmcs

readDouble :: String -> Double
readDouble = read

readInt :: String -> Int
readInt = read

-- | Parse a HMMCompare result string
genParseHMMCompareResult :: GenParser Char st HMMCompareResult
genParseHMMCompareResult = do
    name1 <-  many1 (noneOf " ")
    _ <- many1 space
    name2 <-  many1 (noneOf " ")
    _ <- many1 space
    score1 <- many1 (noneOf " ")
    _ <- many1 space
    score2 <- many1 (noneOf " ")
    _ <- many1 space
    linkseq <- many1 (oneOf "AGTCUagtcu")
    _ <- many1 space
    _ <- char '['
    nodes1 <- many1 parseMatchedNodes
    _ <- char ']'
    _ <- many1 space
    _ <- char '['
    nodes2 <- many1 parseMatchedNodes
    _ <- char ']'
    newline
    return $ HMMCompareResult name1 name2 (readDouble score1) (readDouble score2) linkseq nodes1 nodes2

-- | Parse indices of matched nodes between models as integers
parseMatchedNodes :: GenParser Char st Int
parseMatchedNodes = do
    nodeNumber <- many1 digit
    optional (char ',')
    return (readInt nodeNumber)

-- | Parser for HMMCompare result strings
getHMMCompareResults :: FilePath -> IO [Either ParseError HMMCompareResult]
getHMMCompareResults filePath = let
        fp = filePath
        doParseLine' = parse genParseHMMCompareResult "genParseHMMCompareResults"
        --doParseLine l = case (doParseLine' l) of
        --    Right x -> x
        --    Left _  -> error "Failed to parse line"
    in do
        fileContent <- fmap lines $ readFile fp
        return $ map doParseLine' fileContent

getModelsNames :: [HMMCompareResult] -> [String]
getModelsNames models = concatMap getModelNames models

getModelNames :: HMMCompareResult -> [String]
getModelNames model = [model1Name model,model2Name model]
