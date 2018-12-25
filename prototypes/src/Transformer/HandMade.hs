{-# LANGUAGE OverloadedStrings #-}

module Transformer.HandMade where 
 
import Data.Text

data LoginError = InvalidEmail 
    | NoSuchUser
    | WrongPassword deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email = 
    case splitOn "@" email of
        [name, domain] -> Right domain
        _              -> Left InvalidEmail        
    
        
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

