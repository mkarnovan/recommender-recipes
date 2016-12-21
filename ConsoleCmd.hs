import Data.List
import Data.String
import Data.Either
import System.Environment
import DataDescription
import System.IO.Unsafe
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import TheLoader


-- формат команд
-- print_recipes_by_ingredients ingr1 ingr2 ingr3 ...
-- print_recipe_by_name name_of_recipe
-- filter_all_by_cooktime cook_time
-- sign_up  //регистрация
-- sign_in login pwd //вход
-- help

type Filename = String
-- type Ingredients = [String]
-- type Time = Int -- Minutes
-- type Login = String
-- type Pwd = String
-- data Recipe = Recipe IdUser Rating Name Ingredients Time Description

data GenParams = PrintRecipeByIngr Ingredients |
                 PrintRecipeByName Name |
                 FilterAll Time |
                 FilterFound Time |
                 SignIn Login Pwd |
                 SignUp Login Pwd|
                 Help |
                 Quit

-- TODO разобраться с error (выходит ли из приложения)
-- сделать устойчивую проверку на ошибки

--Глобальная переменная базы рецептов
--Запись: giveMeBase "base.txt" >>= atomically.writeTVar globalRecipes
--Чтение: readTVarIO globalRecipes
globalRecipes :: TVar [Recipe]
globalRecipes = unsafePerformIO $ newTVarIO []

globalAccounts :: TVar [User]
globalAccounts = unsafePerformIO $ newTVarIO []

globalSignedID :: TVar Int
globalSignedID = unsafePerformIO $ newTVarIO (-1)

parseTask :: [String] -> Either String GenParams
parseTask [] = Left "Incorrect command format"
parseTask (mode : xs)
 |mode == "print_recipes_by_ingredients" = Right (PrintRecipeByIngr xs)
 |mode == "print_recipe_by_name" = Right (PrintRecipeByName $ first_arg xs)
 |mode == "filter_all_by_cooktime" = Right (FilterAll (read (first_arg xs) :: Int))
 |mode == "sign_up" = Right (SignUp (first_arg xs) (pwd xs))
 |mode == "sign_in" = Right (SignIn (first_arg xs) (pwd xs))
 |mode == "help" = Right (Help)
 |mode == "quit" = Right (Quit)
 |otherwise = Left "Incorrect command format"
    where
        first_arg xs = head xs
        pwd [x : password] = password
        pwd _ = error "incorrect data format"

-------------- Поиск по ингредиентам -----------------
sortByOverlap (a,b) (c,d)
    | b > d = LT
    |otherwise = GT

getRecipesByIngr :: Ingredients -> [Recipe] -> [Recipe]
getRecipesByIngr xs rs = map fst $ sortBy sortByOverlap (foldl step [] rs)
    where
        step ls (Recipe iD rat name ingr t d)
            |overlap > 0 = ((Recipe iD rat name ingr t d), overlap) : ls
            |otherwise = ls
                where
                    overlap = length( ingr `intersect` xs )

getShortDescr :: Recipe -> String
getShortDescr (Recipe iD rat name ingr t d) = name ++ " (" ++
 (foldl1 (\acc cur -> acc ++ ", " ++ cur) ingr)
 ++ ") "++ (show t) ++ " минут"

getFullDescr :: Recipe -> String
getFullDescr (Recipe iD rat name ingr t d) = name ++ ". "  ++ d
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
            print "Sucsess!"
        else print "Login already exists"

--------------------------------------------------
--------------Вход в систему ---------------------

-- TODO что делать после входа в систему? запомнить ID?

funcSingIn :: Login -> Pwd -> [User] -> IO ()
funcSingIn login pwd base = do
    if logInBase login base
        then do
            print "Sucsess!"
        else print "Login doesn't exist"
-----------------------------------------------------

readBase :: GenParams -> IO ()
readBase (PrintRecipeByIngr xs) = do
    if not (null found)
        then do
            putStrLn $ unlines $ map getShortDescr found
            putStrLn "Для получения подробного описания введите номер рецепта"
            x <- getLine
            putStrLn $ getFullDescr (found !! ((read x :: Int)-1))
    else putStrLn "Нет рецепта с заданными ингредиентами"
        where
            found = getRecipesByIngr xs (unsafePerformIO (giveMeBase "base.txt"))

readBase (PrintRecipeByName name) = do
    let (Recipe idu rat nam ingr t desc) = head (filter (\(Recipe _ _ name1 _ _ _) -> name == name1 ) (unsafePerformIO (giveMeBase "base.txt")))
    putStrLn nam
    putStrLn desc

readBase (FilterAll time) = do
    let xs = filter (\(Recipe _ _ _ _ t _) -> t <= time ) (unsafePerformIO (giveMeBase "base.txt"))
    putStrLn $ unlines $ map toStr' xs
    where
            toStr' (Recipe idu rat name _ t' _) = (findLogIn idu) ++ " " ++ (show rat) ++ " " ++ name ++ " " ++ (show t')
            findLogIn n = getLog (head ( filter (\(User n' _ _) -> n == n') (unsafePerformIO (giveMeAccounts "accounts.txt"))))
            getLog (User _ s _) = s

readBase (SignUp login pwd) = do
    funcSingUp login pwd (unsafePerformIO (giveMeAccounts "accounts.txt"))

readBase (SignIn login pwd) = do
    funcSingIn login pwd (unsafePerformIO (giveMeAccounts "accounts.txt"))

readBase (Help) = do
    putStrLn "Введите следующие команды"
    putStrLn "print_recipes_by_ingredients     - выдает список рецептов по указанным ингредиентам"
    putStrLn "print_recipe_by_name             - выдает описание рецепта по названию"
    putStrLn "filter_all_by_cooktime           - выдает список рецептов, время готовки которых <= указанного числа минут"
    putStrLn "sign_up                          - входит в систему под указанным логином"
    putStrLn "sign_in                          - регистрация пользователя с вводимым логином и паролем"
    putStrLn "quit                             - выход из программы"

helpForSignIn = do
    putStrLn "Введите следующие команды"
    putStrLn "print_recipes_by_ingredients     - выдает список рецептов по указанным ингредиентам"
    putStrLn "print_recipe_by_name             - выдает описание рецепта по названию"
    putStrLn "filter_all_by_cooktime           - выдает список рецептов, время готовки которых <= указанного числа минут"
    putStrLn "sign_out                         - выход из системы"
--
--
askForCommand = do
    putStrLn "Bведите команду (help для посмотра списка доступных команд)"
    l <- getLine
    case parseTask (words l) of
        Right (Quit) -> do
            putStrLn ""
        Right gp -> do
            readBase gp
            askForCommand
        Left str -> do
                    print str
                    askForCommand
