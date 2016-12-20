module DataDescription where

type IdUser = Int;
type Rating = Int;
type Name = String;
type Ingredients = [String]
type Time = Int -- Minutes
type Description = String
type Login = String
type Pwd = String

data Recipe = Recipe IdUser Rating Name Ingredients Time Description deriving Show
<<<<<<< HEAD
<<<<<<< HEAD

=======
>>>>>>> origin/master
=======
>>>>>>> origin/master
data User = User IdUser Login Pwd
