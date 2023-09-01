-- Creating typeclasses and instances

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno :: Int -> Bool
    yesno 0 = False
    yesno _ = True

instance YesNo String where
    yesno :: String -> Bool
    yesno "" = False
    yesno _  = True 

instance YesNo Bool where
    yesno :: Bool -> Bool
    yesno = id

instance YesNo [a] where
    yesno :: [a] -> Bool
    yesno [] = False
    yesno _  = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesNoValue yesResult noResult = if yesno yesNoValue then yesResult else noResult