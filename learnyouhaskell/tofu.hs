class Tofu t where  
    tofu :: j a -> t a j

data Frank a b  = Frank {frankField :: b a} deriving (Show)  

instance Tofu Frank where  
    tofu x = Frank x

-- > tofu (Just 'a') :: Frank Char Maybe
-- Frank {frankField = Just 'a'}

-- > tofu [1] :: Frank Int []
-- Frank {frankField = [1]}
