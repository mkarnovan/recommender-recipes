module TheLoader where

import DataDescription
import TheParser
import Data.Char (ord)
import System.IO
import qualified System.IO.Strict as Strict 
import Control.Monad

--для работы рашн символов hSetEncoding stdin utf8
checkBase:: Int -> String -> Bool
checkBase chksum str = chksum == (foldl (\acc cur -> (acc + cur^2) `mod` 115249) 
                                 111 (map ord str))

encryptBase:: String -> String
encryptBase str = show (foldl (\acc cur -> (acc + cur^2) `mod` 115249) 
                                 111 (map ord str))++'\n':str

replaceFile:: FilePath -> IO ()
replaceFile fp = Strict.readFile fp >>= (\x -> return $ encryptBase x) >>= writeFile fp                               

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

--show для user-а
userToString:: User -> String
userToString (User id name pass) = show id ++ " " ++ name ++ " " ++ pass

--show для recipe-а
recipeToString:: Recipe -> String
recipeToString (Recipe id rating name ingr time desc) = 
    show id ++ ";" ++ show rating ++ ";" ++ name ++ ingrlist ++ show time ++ desc
    where
        ingrlist = foldl (\acc cur -> acc ++ ", " ++ cur) (head ingr) (tail ingr)


--Запись аккаунтов обратно в файл
saveAccounts::FilePath -> [User] -> IO ()
saveAccounts fp ubase = writeFile fp $ encryptBase $ unlines $ 
                        map userToString (tail ubase)

--Запись аккаунтов обратно в файл
--saveRecipes::FilePath -> [User] -> IO ()
--saveRecipes fp ubase = writeFile fp $ encryptBase $ unlines $                         


