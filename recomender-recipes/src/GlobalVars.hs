module GlobalVars where

import System.IO.Unsafe
import Data.IORef
import DataDescription
import TheLoader
import Data.List

sortRec :: [Recipe]-> IO [Recipe]
sortRec xs = return $ sortBy sorting xs
  where
    sorting (Recipe _ rat1 _ _ _ _) (Recipe _ rat2 _ _ _ _) = compare rat2 rat1

--путь к аккунтам
accBaseFpath :: String
accBaseFpath = "accounts.txt"

--путь к базе
recBaseFpath :: String
recBaseFpath = "base.txt"

--Глобальная переменная базы рецептов
globalRecipes :: IORef [Recipe]
globalRecipes = unsafePerformIO $ newIORef []

--Глобальная переменная базы аккаунтов
globalAccounts :: IORef [User]
globalAccounts = unsafePerformIO $ newIORef []

--declareMVar "my-global-some-var" 0

--Глобальная переменная ID авторизованного пользователя
globalSignedID :: IORef Int
globalSignedID = unsafePerformIO $ newIORef (-1)

--Загрузка баз в глобальные переменные
loadBases :: IO ()
loadBases = giveMeAccounts accBaseFpath >>= writeIORef globalAccounts >>
            giveMeBase recBaseFpath >>= sortRec >>= writeIORef globalRecipes

--Сохранение баз в файл
saveBases :: IO ()
saveBases = readIORef globalAccounts >>= saveAccounts accBaseFpath >>
            readIORef globalRecipes >>= saveBase recBaseFpath
