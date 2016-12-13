
type IdUser = Int;
type Rating = Int;
type Name = String;
type Ingredients = [String]
type Time = Int -- Minutes
type Description = String
type Login = String

data Data = Data IdUser Rating Name Ingredients Time Description

data User = User IdUser Login
