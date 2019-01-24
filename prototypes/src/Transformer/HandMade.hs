{-# LANGUAGE OverloadedStrings #-}

module Transformer.HandMade where 
 
import Data.Text
import qualified Data.Text.IO as T

data LoginError = InvalidEmail 
    | NoSuchUser
    | WrongPassword deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email = 
    case splitOn "@" email of
        [name, domain] -> Right domain
        _              -> Left InvalidEmail        
    
-- Combine `Either` and `IO`, and make it a monad                
newtype EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}

instance Functor (EitherIO e) where 
    fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where 
    pure = EitherIO . pure . pure
    mf <*> ma = EitherIO $ pure (<*>) <*> rawF <*> rawA
        where rawF = runEitherIO mf
              rawA = runEitherIO ma  

instance Monad (EitherIO e) where
    return = pure
    ma >>= f = EitherIO $ runEitherIO ma >>= either (return . Left) (runEitherIO . f)

-- Use our hand-made monad    
getToken :: EitherIO LoginError Text
getToken = do
    EitherIO (fmap Right (T.putStrLn "Enter email address:"))
    input <- EitherIO (fmap Right T.getLine)
    EitherIO (return (getDomain input))


