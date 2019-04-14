module Main where

import Control.Monad (when)

import GoatLang.LALRParser (parse)
import GoatLang.Parser (parseProgram)
import GoatLang.AST

main
  = do
      s <- getContents
      
      let parsed = parseProgram "" s
      tdast <- case parsed of
        Left err  -> return (GoatProgram [])
        Right ast -> return ast
      
      let buast = parse s

      when (buast /= tdast) $ do
          putStrLn $ "Bottom-Up:" ++ show buast
          putStrLn $ "Top-Down: " ++ show tdast
      