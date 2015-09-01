{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

import qualified Text.Parsec.Text as P
import qualified Text.ParserCombinators.Parsec as PC
import Control.Monad.Identity (Identity)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import System.Environment

import qualified QuadTree as QT


data City = City Text.Text Double Double deriving (Show)


comma :: P.Parser ()
comma = PC.char ',' >> return ()


pseudoDouble = PC.many $ PC.oneOf "+-.0123456789"
textField :: P.Parser String
textField = PC.many (PC.noneOf ",\n") PC.<?> "textField"

-- needs to read despite missing start-digit (this implementation does only support a subset of all double string representations)
readBadDouble :: P.Parser Double
readBadDouble = do
    sign <- PC.option ' ' (PC.char '-')
    integer_part <- PC.option "0" (PC.many1 PC.digit)
    PC.option '.' $ PC.char '.'
    fractional_part <- PC.option "0" (PC.many1 PC.digit)

    return $ read $ sign : integer_part ++ "." ++ fractional_part


city :: P.Parser City
city = do
    country <- textField
    comma
    city <- textField
    comma
    accentCity <- textField
    comma
    region <- textField
    comma
    population <- pseudoDouble  -- Maybe
    comma
    latitude <- readBadDouble
    comma
    longitude <- readBadDouble

    return $ City (Text.pack city) latitude longitude


cities :: P.Parser [City]
cities = do
    skipHeader
    cities <- PC.many city
    return  cities
        where skipHeader = (PC.many (PC.noneOf "\n")) >> PC.char '\n' >> return ()


parseLine :: Text.Text -> IO ()
parseLine line = do
    print $ render $ PC.parse city "city" line
    where render (Right value) = value
          render (Left err) = error ("parse failed" ++ show err)

main = do
    content <- Text.readFile "data/worldcitiespop.utf8.txt"
    (mapM_ parseLine) $ Prelude.tail $ Text.lines $ content
    --print $ case parse cities "Cities" content of
    --    Left err -> "no match" ++ show err
    --    Right val -> show val

