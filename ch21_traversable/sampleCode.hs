module SampleCode where

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- There's a decoder function that makes
-- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against DB and
-- returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- there's some additional "context initializer",
-- that also has IO side-effects
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
  (Left err) -> return $ Left $ err
  (Right res) -> do
    a <- makeIoOnlyObj res
    return $ Right a



pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)

{-
a :: IO [String]

(>>=) :: m a -> (a -> m b) -> m b
sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
decodeFn :: String -> Either Err SomeObj
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]

traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
mapM :: Monad m => (a -> m b) -> t a -> m (t b)

The key point to note is that all this occurs within a monadic bind function, (>>=) implicit in `do`. 

a :: IO [String] >>= traverse makeIoOnlyObj (mapM decodeFn)

THe (>>=) ensures we map over the IO and `mapM` ensures we map over the [].
So `mapM decodeFn` first produces the intermediate result IO [Either Err SomeObj] which is then flipped inside out to produce IO (Either Err [SomeObj]).

Then `traverse makeIoOnlyObj` first maps the `makeIoOnlyObj` to produce IO (Either (IO Err) (IO [(Some, IoOnlyObj)])) and then applies the `sequenceA` to turn it inside out, producing IO (IO (Either Err [(Some, IoOnlyObj)])).

Finally, since all this occurs within the context of an outer IO, we apply the `Control.Monad.join` implicit in `(>>=)` to reduce the outer layers of IO monads, producing `IO (Either Err [(Some, IoOnlyObj)])` as the type of our final result.

-}

-- Cleaner and with point-free style
pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = (traverse makeIoOnlyObj
              . traverse decodeFn =<<) . fetchFn

{-
This is roughly what happens to the types

1. fetchFn produces IO [String]

2. traverse decodeFn =<< IO [String] produces IO (Either Err [SomeObj])

3. traverse makeIoOnlyObj (IO (Either Err [SomeObj])) produces IO (IO (Either Err [(SomeObj, IoOnlyObj)]))

4. Finally, we use the implicit Control.Monad.join to produce
IO (Either Err [(SomeObj, IoOnlyObj)])

-}