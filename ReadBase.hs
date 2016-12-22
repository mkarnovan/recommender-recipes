module ReadBase where

import DataDescription
import Register
import GlobalVars
import Reciepts
import Control.Concurrent.STM

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

readBase :: GenParams -> IO ()
readBase (PrintRecipeByIngr xs) = readTVarIO globalRecipes >>=
           return .getRecipesByIngr xs >>= (\found -> if not (null found)
           then do
             putStrLn $ unlines $ map getShortDescr found
             putStrLn "Для получения подробного описания введите номер рецепта"
             x <- getLine
             if (isNumber x) then 
               putStrLn $ getFullDescr (found !! ((read x :: Int)-1))
             else putStrLn "Неверный номер"
           else putStrLn "Нет рецепта с заданными ингредиентами")


readBase (PrintRecipeByName name) = do
    recBase <- (readTVarIO globalRecipes)
    let (Recipe idu rat nam ingr t desc) = head (filter (\(Recipe _ _ name1 _ _ _) -> name == name1 ) recBase)
    putStrLn nam
    putStrLn desc

readBase (FilterAll time) = do
    accBase <- (readTVarIO globalAccounts)
    recBase <- (readTVarIO globalRecipes)
    let getLog (User _ s _) = s
    let findLogIn = \n -> getLog (head ( filter (\(User n' _ _) -> n == n') accBase))
    let toStr' = \(Recipe idu rat name _ t' _) -> (findLogIn idu) ++ " " ++ (show rat) ++ " " ++ name ++ " " ++ (show t')
    let xs = filter (\(Recipe _ _ _ _ t _) -> t <= time ) recBase
    putStrLn $ unlines $ map toStr' xs


readBase (SignUp login pwd) =  readTVarIO globalAccounts >>= funcSingUp login pwd

readBase (SignIn login pwd) = readTVarIO globalAccounts >>= funcSingIn login pwd

readBase (Help) = do
    putStrLn "filter_ingr                       - выдает список рецептов по указанным ингредиентам"
    putStrLn "find_by_name                      - выдает описание рецепта по названию"
    putStrLn "filter_time                       - выдает список рецептов, время готовки которых <= указанного числа минут"
    putStrLn "sign_up                          - входит в систему под указанным логином"
    putStrLn "sign_in                          - регистрация пользователя с вводимым логином и паролем"
    putStrLn "quit                             - выход из программы"

