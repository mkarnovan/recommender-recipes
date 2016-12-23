module Recipes where

import Data.List
import Data.List.Split
import Data.IORef
import DataDescription
import GlobalVars

--Добавление рецепта в глобальную базу
addRecipe :: Recipe -> IO ()
addRecipe nRecipe = readIORef globalRecipes >>= (\ee -> writeIORef globalRecipes (nRecipe:ee))

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
--
getShortDescr :: Recipe -> String
getShortDescr (Recipe iD rat name ingr t d) = name ++ " (" ++
 (foldl1 (\acc cur -> acc ++ ", " ++ cur) ingr)
 ++ ") "++ (show t) ++ " минут"

getFullDescr :: Recipe -> String
getFullDescr (Recipe iD rat name ingr t d) = name ++ ". "  ++ d


-------------Добавление рецепта----------------------
addFunc :: String -> IO ()
addFunc s = do
    uid <- (readIORef globalSignedID)
    if (uid /= (-1)) then do
        let [nam, ingr, t', desc] = splitOn ";" s
        let t = read t'
        addRecipe(Recipe uid 0 nam (words ingr) t desc)
    else
        putStrLn "Для добавления рецепта требуется авторизация."
