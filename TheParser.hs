module TheParser where

import Data.List
import System.Environment

import DataDescription

--Парсер строки
strToRecipe ::  String -> Recipe
strToRecipe = undefined


--Готовый список рецептов
linesToRecipes ::  [String] -> [Recipe]
linesToRecipes = map strToRecipe