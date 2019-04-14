module Main where

import Control.Monad (when)

import GoatLang.LALRParser (parse)
import GoatLang.Parser (parseProgram)

main
  = do
      s <- getContents
      let buast = parse s
      let tdast = parseProgram s
      when (buast /= tdast) $ do
          putStrLn $ "Bottom-Up:" ++ show buast
          putStrLn $ "Top-Down: " ++ show tdast
      