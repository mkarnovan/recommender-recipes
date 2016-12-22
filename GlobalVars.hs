module GlobalVars where

import System.IO.Unsafe
import Control.Concurrent.STM
import DataDescription
import TheLoader

--путь к аккунтам
accBaseFpath :: String
accBaseFpath = "accounts.txt"

--путь к базе
recBaseFpath :: String
recBaseFpath = "base.txt"

--Глобальная переменная базы рецептов
globalRecipes :: TVar [Recipe]
globalRecipes = unsafePerformIO $ newTVarIO []

--Глобальная переменная базы аккаунтов
globalAccounts :: TVar [User]
globalAccounts = unsafePerformIO $ newTVarIO []

--declareMVar "my-global-some-var" 0

--Глобальная переменная ID авторизованного пользователя
globalSignedID :: TVar Int
globalSignedID = unsafePerformIO $ newTVarIO (-1)

--Загрузка баз в глобальные переменные
loadBases :: IO ()
loadBases = giveMeAccounts accBaseFpath >>= atomically.writeTVar globalAccounts >>
            giveMeBase recBaseFpath >>= atomically.writeTVar globalRecipes

--Сохранение баз в файл
saveBases :: IO ()
saveBases = readTVarIO globalAccounts >>= saveAccounts accBaseFpath >>
            readTVarIO globalRecipes >>= saveBase recBaseFpath
