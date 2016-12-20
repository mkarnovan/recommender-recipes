module TheLoader where

import DataDescription
import TheParser
import Data.Char (ord)
import System.IO

--для работы рашн символов hSetEncoding stdin utf8

checkBase:: Int -> String -> Bool
checkBase chksum str = chksum == (foldl (\acc cur -> (acc + cur^2) `mod` 115249) 
                                 111 (map ord str))

encryptBase:: String -> String
encryptBase str = show (foldl (\acc cur -> (acc + cur^2) `mod` 115249) 
                                 111 (map ord str))++'\n':str

-- Возвращает базу, если контрольная сумма верна
-- Либо пустой список если база пуста или некорректна
giveMeBase:: FilePath -> IO [Recipe]
giveMeBase fp = do
    content <- readFile fp
    let clines = lines content
    let chksum = read (head clines)::Int
    let body = unwords $ tail clines
    let baseIsCorrect = checkBase chksum body
    if (not baseIsCorrect) then return []
    else
        return $ linesToRecipes $ tail clines




--Возвращает базу аккаунтов
giveMeAccounts:: FilePath -> IO [User]
giveMeAccounts fp = do
    content <- readFile fp
    let clines = lines content
    let chksum = read (head clines)::Int
    let body = unwords $ tail clines
    let baseIsCorrect = checkBase chksum body
    if (not baseIsCorrect) then return []
    else
        return $ linesToUsers $ tail clines        