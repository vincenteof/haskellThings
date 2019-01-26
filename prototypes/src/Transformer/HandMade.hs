{-# LANGUAGE OverloadedStrings #-}

module Transformer.HandMade where 
 
import Data.Text
import qualified Data.Text.IO as T
import Data.Map as M

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

-- Use our hand-made monad, we simplify original implementation using `lift`   
getToken :: EitherIO LoginError Text
getToken = do
    liftIO (T.putStrLn "Enter email address:")
    input <- liftIO T.getLine
    liftEither (getDomain input)

-- What `lift` does is making a monad become a more powerful one    
liftEither :: Either e a -> EitherIO e a
liftEither = EitherIO . return 

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO $ fmap Right x


-- User interface
users :: Map Text Text
users = M.fromList [("example.com", "qwerty123"), ("localhost", "password")]

userLogin :: EitherIO LoginError Text
userLogin = do
    token       <- getToken
    userPwd     <- maybe (liftEither (Left NoSuchUser)) return (M.lookup token users)
    pwd         <- liftIO (T.putStrLn "Enter your password:" >> T.getLine)
    if userPwd == pwd
        then return token
        else liftEither (Left WrongPassword)

printResult :: Either LoginError Text -> IO ()
printResult res = 
    T.putStrLn $ case res of
        Right token         -> append "Logged in with token: " token
        Left InvalidEmail   -> "Invalid email address entered"
        Left NoSuchUser     -> "No user with that email exists"
        Left WrongPassword  -> "Wrong password"
