module Register where

import Control.Concurrent.STM
import DataDescription
import GlobalVars
import TheLoader

------------------------------------------------------
-------------- Регистрация ---------------------------

addNewUser :: Login -> Pwd -> [User] -> [User]
addNewUser l p base = base ++ [User (length base + 1) l p]


getLogins :: [User] -> [Login]
getLogins = foldl step []
    where
        step ls (User iD l p) = l : ls

logInBase :: Login -> [User] -> Bool
logInBase l base = elem l logs
    where
        logs = getLogins base

funcSingUp :: Login -> Pwd -> [User] -> IO ()
funcSingUp login pwd base = do
    if not (logInBase login base)
        then do
            print $ last $ addNewUser login pwd base
            print "Sucсess!"
        else print "Login already exists"

--------------------------------------------------
--------------Вход в систему ---------------------

-- TODO что делать после входа в систему? запомнить ID?
getIdByLog :: Login -> [User] -> IdUser
getIdByLog login base = foldl step 0 base
    where
      step iD (User idu l p)
        |l == login = idu
        | otherwise = iD

funcSingIn :: Login -> Pwd -> [User] -> IO ()
funcSingIn login pwd base = do
    --putStrLn $ unlines $ map userToString base
    if logInBase login base
        then do
            atomically $ writeTVar globalSignedID (getIdByLog login base)
            putStrLn $ "Привет, " ++ login ++ "!"
        else putStrLn "Такого логина не существует, для регистрации используйте sign_up"
-----------------------------------------------------
