-- | Given a type that has an instance of Applicative, specialize the
-- types of the methods. Test your specialization in the REPL. One way
-- to do this is to bind aliases of the typeclass methods to “more
-- concrete” types that have the type we told you to fill in.


-- 1.
-- Type:  []
-- Methods: 

pure :: Char -> [] Char
(<*>) :: [] (Char -> Int) -> [] Char -> [] Int
-- pure 'c'
-- [ord] <*> "c"

-- 2.
-- Type: IO
-- Methods

pure :: Char -> IO Char
(<*>) :: IO (Char -> Int) -> IO Char -> IO Int
-- pure 'c'
-- pure ord <*> pure 'c'



-- 3.
-- Type (,) a
-- Methods

--  TODO ???

pure :: Char -> (,) a Char
(<*>) :: (,) a (Char -> Int) -> (,) a Char -> (,) a Int

--4.
-- Type (->) e
-- Methods

-- TODO ???

pure :: a -> ? a
(<*>) :: ? (a -> b) -> ? a -> ? b

