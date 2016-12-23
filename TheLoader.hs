module TheLoader where

import DataDescription
import TheParser
import Data.Char (ord)
import System.IO
import qualified System.IO.Strict as Strict
import Control.Monad
import Control.Exception
import Data.Typeable

--для работы рашн символов hSetEncoding stdin utf8
checkBase:: Int -> String -> Bool
checkBase chksum str = chksum == (foldl (\acc cur -> (acc + cur^2) `mod` 115249)
                                 111 (map ord str))

encryptBase:: String -> String
encryptBase str = show (foldl (\acc cur -> (acc + cur^2) `mod` 115249)
                                 111 (map ord str))++'\n':str

replaceFile:: FilePath -> IO ()
replaceFile fp = Strict.readFile fp >>= (\x -> return $ encryptBase x) >>= writeFile fp


chk:: String -> Int
chk str = foldl (\acc cur -> (acc + cur^2) `mod` 115249)
                                 111 (map ord str)


-- Возвращает базу, если контрольная сумма верна
-- Либо пустой список если база пуста или некорректна
giveMeBase:: FilePath -> IO [Recipe]
giveMeBase fp = do
    content <- readFile fp
    let clines = lines content
    let chksum = read (head clines)::Int
    let body = unlines $ tail clines
    let baseIsCorrect = checkBase chksum body
    --putStrLn $ show (chk body) ++ " " ++ show chksum
    if (not baseIsCorrect) then throw IncorrectBase
    else
    	return $ linesToRecipes $ tail clines


--Возвращает базу аккаунтов
giveMeAccounts:: FilePath -> IO [User]
giveMeAccounts fp = do
    content <- readFile fp
    let clines = lines content
    let chksum = read (head clines)::Int
    let body = unlines $ tail clines
    let baseIsCorrect = checkBase chksum body
    --putStrLn $ show (chk body) ++ " " ++ show chksum
    if (not baseIsCorrect) then throw IncorrectBase
    else
        return $ linesToUsers $ tail clines

--Запись аккаунтов обратно в файл
saveAccounts::FilePath -> [User] -> IO ()
saveAccounts fp ubase = writeFile fp $ encryptBase $ unlines $
                        map userToString (tail ubase)

--Запись аккаунтов обратно в файл
saveBase::FilePath -> [Recipe] -> IO ()
saveBase fp rbase = writeFile fp $ encryptBase $ unlines $
		       map recipeToString rbase
