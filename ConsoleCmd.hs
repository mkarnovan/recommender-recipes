import Data.List
import Data.String
import System.Environment
import DataDescription

-- формат команд
-- print_recipes_by_ingredients ingr1 ingr2 ingr3 ...
-- print_recipe_by_name name_of_recipe
-- filter_all_by_cooktime cook_time
-- filter_found_by_cooktime cook_time //фильтруем по времени приготовления результат Print_recipes_by_ingredients
-- sign_up  //регистрация
-- sign_in login pwd //вход
-- help

type Filename = String
-- type Ingredients = [String]
-- type Time = Int -- Minutes
-- type Login = String
-- type Pwd = String

data GenParams = PrintRecipeByIngr Ingredients |
                 PrintRecipeByName Name |
                 FilterAll Time |
                 FilterFound Time |
                 SignIn Login Pwd |
                 SingUp |
                 Help

-- TODO разобраться с error (выходит ли из приложения)
-- сделать устойчивую проверку на ошибки

parseCommand :: [String] -> Either String GenParams
parseTask [] = Nothing
parseTask (mode : xs)
 |mode == "print_recipes_by_ingredients" = Right (PrintRecipeByIngr xs)
 |mode == "print_recipe_by_name" = Right (PrintRecipeByName $ first_arg xs)
 |mode == "filter_all_by_cooktime" = Right (FilterAll read (first_arg xs) :: Int)
 |mode == "filter_found_by_cooktime" = Right (FilterFound read (first_arg xs) :: Int)
 |mode == "sign_up" = Right (SignUp $ first_arg xs)
 |mode == "sign_in" = Right (SignIn (first_arg xs) (pwd xs))
 |mode == "help" = Right (Help)
 |otherwise = Left "Incorrect command format"
    where
        first_arg xs = head xs
        first_arg [] = error "incorrect data format"
        pwd [x : password] = password
        pwd _ = error "incorrect data format"



readBase :: GenParams -> IO ()
readBase (PrintRecipeByIngr xs) = undefined
readBase (PrintRecipeByName name) = undefined
readBase (FilterAll time) = undefined
readBase (FilterFound time) = undefined
readBase (SignUp login) = undefined
readBase (SignIn login pwd) = undefined
readBase (Help) = undefined 


askForCommand = do
    putStrLn "Bведите команду (help для посмотра списка доступных команд)"
    l <- getLine
    case parseCommand (words l) of
        Right gp -> readBase gp
        Left str -> do
                    printLn str
                    askForCommand
