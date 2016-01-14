-- http://learnyouahaskell.com/starting-out
--
-- how to define a function
doubleMe x = x + x

-- doubleUs x y = x*2 + y*2
-- you can use a function to make a bigger function
doubleUs x y = doubleMe x + doubleMe y

-- if statement, the x = has to be top line
doubleSmallNumber x = if x > 100 
    then x 
    else x*2

-- a function name can include an apostrophe because why not
-- "We usually use ' to either denote a strict version of a function (one that isn't lazy) or a slightly modified version of a function or a variable"
-- but they can't start with capitals
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1


