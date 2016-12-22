module TheParser where

import Data.List
import Data.List.Split
import System.Environment

import DataDescription

--Парсер строки
strToRecipe :: String -> Recipe
strToRecipe str = Recipe idu rat name ingr t desc
	where 
		[idu',rat',name',ingr',t',desc'] = splitOn ";" str
		idu = read idu'
		rat = read rat'
		name = name'
		ingr = splitOn ", " ingr'
		t = read t'
		desc = desc'

--Готовый список рецептов
linesToRecipes ::  [String] -> [Recipe]
linesToRecipes = map strToRecipe


--Парсер строки
strToUser :: String -> User
strToUser str = User id uname upass
	where 
		[id',uname,upass] = words str
		id = read id'

--Готовый список аккаунтов
linesToUsers ::  [String] -> [User]
linesToUsers str = User 0 "admin" "0000":map strToUser str

--show для user-а
userToString:: User -> String
userToString (User id name pass) = show id ++ " " ++ name ++ " " ++ pass

--show для recipe-а
recipeToString:: Recipe -> String
recipeToString (Recipe id rating name ingr time desc) = 
        foldl1 (\acc cur -> acc ++ ';':cur) 
	[show id, show rating, name, ingrlist, show time, desc]
    --show id ++ ";" ++ show rating ++ ";" ++ name ++ ";" ++ ingrlist ++ ";" ++ show time  ++";" ++ desc
    where
        ingrlist = foldl1 (\acc cur -> acc ++ ", " ++ cur) ingr
