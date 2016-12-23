import DataDescription
import TheParser
import TheLoader
import Register
import GlobalVars
import Recipes
import ReadBase
import Control.Exception
import Data.Typeable

askForCommand = do
    putStrLn "Bведите команду (help для посмотра списка доступных команд)"
    l <- getLine
    case parseTask (words l) of
        Right (Quit) -> do
            putStrLn "Пока! :)"
        Right gp -> do
            readBase gp
            askForCommand
        Left str -> do
                    putStrLn str
                    askForCommand

exceptionFunc = do
    putStrLn "Несанкционированные изменения, база заблокирована. Позовите администратора или введите exit"
    l <- getLine
    case (words l) of
        ["replaceFile", base] -> do
             replaceFile base
             main
        ["exit"] -> putStrLn "Пока! :)"
        _ -> exceptionFunc


main = do
    loadBases
    askForCommand
    saveBases
    `catch` \e -> do
        print (e :: FormatException)
        exceptionFunc
