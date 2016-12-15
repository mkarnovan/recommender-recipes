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