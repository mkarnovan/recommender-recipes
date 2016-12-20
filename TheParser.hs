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