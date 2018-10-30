const <$> Just "Hello" <*> pure "World"

(,,,) <$> Just 90 <*> Just 10 <*> Just "tierness" <*> pure [1, 2, 3]
