import Data.List
import Data.String
import Data.Either
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
-- data Recipe = Recipe IdUser Rating Name Ingredients Time Description

data GenParams = PrintRecipeByIngr Ingredients |
                 PrintRecipeByName Name |
                 FilterAll Time |
                 FilterFound Time |
                 SignIn Login Pwd |
                 SignUp Login Pwd |
                 Help

-- TODO разобраться с error (выходит ли из приложения)
-- сделать устойчивую проверку на ошибки

parseTask :: [String] -> Either String GenParams
parseTask [] = Left "Incorrect command format"
parseTask (mode : xs)
 |mode == "print_recipes_by_ingredients" = Right (PrintRecipeByIngr xs)
 |mode == "print_recipe_by_name" = Right (PrintRecipeByName $ first_arg xs)
 |mode == "filter_all_by_cooktime" = Right (FilterAll (read (first_arg xs) :: Int))
 |mode == "filter_found_by_cooktime" = Right (FilterFound (read (first_arg xs) :: Int))
 |mode == "sign_up" = Right (SignUp $ first_arg xs)
 |mode == "sign_in" = Right (SignIn (first_arg xs) (pwd xs))
 |mode == "help" = Right (Help)
 |otherwise = Left "Incorrect command format"
    where
        first_arg xs = head xs
        pwd [x : password] = password
        pwd _ = error "incorrect data format"
--
-- -- data Recipe = Recipe IdUser Rating Name Ingredients Time Description
--
sortByOverlap (a,b) (c,d)
    | b > d = LT
    |otherwise = GT
--
--
getRecipesByIngr :: Ingredients -> [Recipe] -> [Recipe]
getRecipesByIngr xs rs = map fst $ sortBy sortByOverlap (foldl step [] rs)
    where
        step ls (Recipe iD rat name ingr t d)
            |overlap > 0 = ((Recipe iD rat name ingr t d), overlap) : ls
            |otherwise = ls
                where
                    overlap = length( ingr `intersect` xs )


readBase :: GenParams -> IO ()
readBase (PrintRecipeByIngr xs) = do
    content <- readFile "base.txt"
    print " "
    -- print $ getRecipesByIngr xs (linesToRecipes content)

readBase (PrintRecipeByName name) = do
	let Recipe(idu rat nam ingr t desc) = head $ filter (\Recipe(_ _ name1 _ _ _) -> name == name1 ) 
	putStrLn nam
	putStrLn desc

readBase (FilterAll time) = do
	let xs = filter (\Recipe(_ _ _ _ t _) -> t == time ) 
	mapM print' xs
	where
		print' Recipe(idu rat name _ t' _) = do
			putStrLn idu + " " + rat + " " + name + " " + t'
	
readBase (FilterFound time) = undefined
readBase (SignUp login pwd) = undefined
readBase (SignIn login pwd) = undefined
readBase (Help) = undefined
--
--
askForCommand = do
    putStrLn "Bведите команду (help для посмотра списка доступных команд)"
    l <- getLine
    case parseTask (words l) of
        Right gp -> readBase gp
        Left str -> do
                    print str
                    askForCommand
