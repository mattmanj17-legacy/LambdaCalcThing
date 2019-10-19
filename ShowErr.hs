{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module ShowErr where

import Util
import LambdaAst
import Control.Monad.Reader

startRedHighlightAt :: Int -> String -> String
startRedHighlightAt n = 
  insert (n - 1) "\x1b[31m"

endHighlightAt :: Int -> String -> String
endHighlightAt n =
  insert (n - 1) "\x1b[0m"

highlightLinesFromColumns :: Int -> Int -> [String] -> [String]
highlightLinesFromColumns start end =
  (mapFirst (startRedHighlightAt start)) . (mapLast (endHighlightAt end))

focusLines :: SourceInfo -> [String] -> [String]
focusLines srcInf =
  (take cLineTake) . (drop cLineDrop)
  where
    startLine = getStartLine srcInf
    endLine = getEndLine srcInf
    cLineDrop = startLine - 1
    cLineTake = endLine - startLine + 1

highlightLinesFromSrcInf :: SourceInfo -> [String] -> [String]
highlightLinesFromSrcInf srcInf fileLines =
  highlightLinesFromColumns startChar endChar focusedLines
  where
    startChar = getStartColumn srcInf
    endChar = getEndColumn srcInf
    focusedLines = focusLines srcInf fileLines

errorStrAt :: 
  (Monad m) => 
  AstR -> 
  String -> 
  ReaderT [String] m String
errorStrAt ast strMsg = do
  fileLines <- ask
  let highlightedLines = highlightLinesFromSrcInf srcInf fileLines
  return $ unlines $ infoStr:highlightedLines
  where
    srcInf = getSrcInf ast
    infoStr = show srcInf ++ " " ++ strMsg    