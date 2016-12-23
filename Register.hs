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
            putStrLn $ "Все отлично!" ++ login ++ ", вы зарегистрировались."
        else putStrLn $ "Такой логин уже существует."

--------------------------------------------------
--------------Вход в систему ---------------------

checkUser :: Login -> Pwd -> [User] -> Bool
checkUser login pwd base = not ( null ( filter (\(User _ l p) -> l == login && p == pwd) base) )

getIdByLog :: Login -> [User] -> IdUser
getIdByLog login base = foldl step 0 base
    where
      step iD (User idu l p)
        |l == login = idu
        | otherwise = iD

funcSingIn :: Login -> Pwd -> [User] -> IO ()
funcSingIn login pwd base = do
    if checkUser login pwd base
        then do
            atomically $ writeTVar globalSignedID (getIdByLog login base)
            putStrLn $ "Привет, " ++ login ++ "!"
        else putStrLn "Такого логина и пароля не существует, для регистрации используйте sign_up"

funcSingOut :: IO ()
funcSingOut = do
    atomically$ writeTVar globalSignedID (-1)
    putStrLn $ "Вы вышли из учетной записи."
-----------------------------------------------------
