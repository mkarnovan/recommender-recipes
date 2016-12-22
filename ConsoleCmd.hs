import Data.List
import Data.String
import Data.Either
import Data.List.Split
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
                 Add String |
                 SignOut |
                 Help |
                 Quit

-- TODO разобраться с error (выходит ли из приложения)
-- сделать устойчивую проверку на ошибки

accBaseFpath :: String
accBaseFpath = "accounts.txt"

recBaseFpath :: String
recBaseFpath = "base.txt"




--Глобальная переменная базы рецептов
--Запись: giveMeBase "base.txt" >>= atomically.writeTVar globalRecipes
--Чтение: readTVarIO globalRecipes


globalRecipes :: TVar [Recipe]
globalRecipes = unsafePerformIO $ newTVarIO []

globalAccounts :: TVar [User]
globalAccounts = unsafePerformIO $ newTVarIO []
--declareMVar "my-global-some-var" 0
globalSignedID :: TVar Int
globalSignedID = unsafePerformIO $ newTVarIO (-1)

loadBases :: IO ()
loadBases = giveMeAccounts accBaseFpath >>= atomically.writeTVar globalAccounts >>
            giveMeBase recBaseFpath >>= atomically.writeTVar globalRecipes

saveBases :: IO ()
saveBases = readTVarIO globalAccounts >>= saveAccounts accBaseFpath >>
            readTVarIO globalRecipes >>= saveBase recBaseFpath

addRecipe :: Recipe -> IO ()
addRecipe nRecipe = readTVarIO globalRecipes >>= (\ee -> atomically (writeTVar globalRecipes (nRecipe:ee)))
----------------------------------------------------
parseTask :: [String] -> Either String GenParams
parseTask [] = Left "Incorrect command format"
parseTask (mode : xs)
 |mode == "filter_ingr" = Right (PrintRecipeByIngr xs)
 |mode == "find_by_name" = Right (PrintRecipeByName $ first_arg xs)
 |mode == "filter_time" = Right (FilterAll (read (first_arg xs) :: Int))
 |mode == "sign_up" = Right (SignUp (first_arg xs) (pwd xs))
 |mode == "sign_in" = Right (SignIn (first_arg xs) (pwd xs))
 |mode == "add" = Right (Add (unlines xs))
 |mode == "help" = Right (Help)
 |mode == "quit" = Right (Quit)
 |mode == "sign_out" = Right (SignOut)
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
            putStrLn $ "Привет, " ++ login ++ "!"
        else putStrLn "Такого логина не существует, для регистрации используйте sign_up"
-----------------------------------------------------

----------------Добавление рецепта-------------------

strToRecForSignIn :: String -> Either String Recipe
strToRecForSignIn s
    | idu == (-1) = Left "незарегистрированный пользователь"
    | otherwise = Right (Recipe idu 0 nam (words ingr) t desc)
    where
        idu = (unsafePerformIO(readTVarIO globalSignedID))
        [nam, ingr, t', desc] = splitOn ";" s
        t = read t'
-----------------------------------------------------

readBase :: GenParams -> IO ()
readBase (PrintRecipeByIngr xs) = readTVarIO globalRecipes >>=
           return .getRecipesByIngr xs >>= (\found -> if not (null found)
           then do
             putStrLn $ unlines $ map getShortDescr found
             putStrLn "Для получения подробного описания введите номер рецепта"
             x <- getLine
             putStrLn $ getFullDescr (found !! ((read x :: Int)-1))
           else putStrLn "Нет рецепта с заданными ингредиентами")


readBase (PrintRecipeByName name) = do
    c <- (readTVarIO globalSignedID)
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

readBase (Add str) = do
    case strToRecForSignIn str of
        Right rec -> do
            putStrLn "Ваш рецепт добавлен в базу"
        Left str -> do
            putStrLn str

readBase (SignUp login pwd) = do
    funcSingUp login pwd (unsafePerformIO (giveMeAccounts "accounts.txt"))

readBase (SignIn login pwd) = do
    funcSingIn login pwd (unsafePerformIO (giveMeAccounts "accounts.txt"))

-- readBase (SignOut) = do
--

readBase (Help) = do
    putStrLn "Введите следующие команды"
    putStrLn "filter_ingr                       - выдает список рецептов по указанным ингредиентам"
    putStrLn "find_by_name                      - выдает описание рецепта по названию"
    putStrLn "filter_time                       - выдает список рецептов, время готовки которых <= указанного числа минут"
    putStrLn "sign_up                           - входит в систему под указанным логином"
    putStrLn "sign_in                           - регистрация пользователя с вводимым логином и паролем"
    putStrLn "quit                              - выход из программы"

helpForSignIn = do
    putStrLn "Введите следующие команды"
    putStrLn "filter_ingr                       - выдает список рецептов по указанным ингредиентам"
    putStrLn "find_by_name                      - выдает описание рецепта по названию"
    putStrLn "filter_time                       - выдает список рецептов, время готовки которых <= указанного числа минут"
    putStrLn "add                               - добавление нового рецепта"
    putStrLn "sign_out                          - выход из системы"
--
--
askForCommand = do
    putStrLn "Bведите команду (help для посмотра списка доступных команд)"
    l <- getLine
    case parseTask (words l) of
        Right (Quit) -> do
            putStrLn "Пока! :) "
        Right gp -> do
            readBase gp
            askForCommand
        Left str -> do
                    print str
                    askForCommand

main = loadBases >> askForCommand >> saveBases
