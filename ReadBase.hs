module ReadBase where

import DataDescription
import Register
import GlobalVars
import Recipes
import Data.IORef

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

readBase :: GenParams -> IO ()
readBase (PrintRecipeByIngr xs) = do
           recBase <- (readIORef globalRecipes)
           let found = getRecipesByIngr xs recBase
           let func (n,s) = (show n) ++ ") " ++ s
           if not (null found)
           then do
             let strs = map getShortDescr found
             putStrLn $ unlines $ map func $ zipWith (,) [1..(length strs)] strs
             putStrLn "Для получения подробного описания введите номер рецепта"
             x <- getLine
             if (isNumber x) then do
               let curRec = (found !! ((read x :: Int)-1))
               let (Recipe idu rat name ingr t desc) = curRec
               let newBase = (Recipe idu (rat+1) name ingr t desc) : (filter (\(Recipe _ _ name1 _ _ _) -> name1 /= name) recBase) --чтоб тут заюзать
               writeIORef globalRecipes newBase
               putStrLn $ getFullDescr curRec
             else putStrLn "Неверный номер"
           else putStrLn "Нет рецепта с заданными ингредиентами"



readBase (PrintRecipeByName name) = do
    recBase <- (readIORef globalRecipes)
    let curRec = head (filter (\(Recipe _ _ name1 _ _ _) -> name == name1 ) recBase)
    let (Recipe idu rat nam ingr t desc) = curRec
    let newBase = (Recipe idu (rat+1) nam ingr t desc) : (filter (\(Recipe _ _ name1 _ _ _) -> name1 /= name) recBase)
    writeIORef globalRecipes newBase
    putStrLn nam
    putStrLn desc

readBase (FilterAll time) = do
    accBase <- (readIORef globalAccounts)
    recBase <- (readIORef globalRecipes)
    let getLog (User _ s _) = s
    let findLogIn = \n -> getLog (head ( filter (\(User n' _ _) -> n == n') accBase))
    let toStr' = \(Recipe idu rat name _ t' _) -> (findLogIn idu) ++ " " ++ (show rat) ++ " " ++ name ++ " " ++ (show t')
    let xs = filter (\(Recipe _ _ _ _ t _) -> t <= time ) recBase
    putStrLn $ unlines $ map toStr' xs

readBase (Add str) = addFunc str

readBase (SignUp login pwd) =  readIORef globalAccounts >>= funcSingUp login pwd

readBase (SignIn login pwd) = readIORef globalAccounts >>= funcSingIn login pwd

readBase (SignOut) = funcSingOut

readBase (Help) = do
    putStrLn "filterIngr                       - выдает список рецептов по указанным ингредиентам"
    putStrLn "findByName                       - выдает описание рецепта по названию"
    putStrLn "filterByTime                     - выдает список рецептов, время готовки которых <= указанного числа минут"
    putStrLn "signUp                           - регистрация пользователя с вводимым логином и паролем"
    putStrLn "signIn                           - входит в систему под указанным логином"
    putStrLn "signOut                          - выход из учетной записи"
    putStrLn "quit                             - выход из программы"
