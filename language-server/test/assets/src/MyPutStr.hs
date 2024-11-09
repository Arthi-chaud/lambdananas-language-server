module MyPutStrLn where

myPutStr = putStr

tooManyIfs :: Bool
tooManyIfs = if True then True else if False then False else True
