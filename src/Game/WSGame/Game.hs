module Game.WSGame.Game
  ( Error(..)
  , Env(..)
  , GameM, runGameM
  , throw, throwSoft, throwHard
  , perform
  , liftSTM
  , getConnection
  , getEnv
  , getState, setState, modifyState
  )
  where

import Prelude hiding (log)

import Data.Foldable (traverse_)
import Control.Exception (SomeException)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.CPS hiding (state)
import Control.Concurrent.STM (STM, TVar)
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

data Error
  = SoftError String  -- keep the connection
  | HardError String  -- kill the connection
  deriving (Eq, Ord)

instance Show Error where
  show (SoftError msg) = "soft error: " ++ msg
  show (HardError msg) = "hard error: " ++ msg

data Env st genv conn = Env
  { connection :: conn
  , state :: TVar st
  , gameEnv :: genv
  }

newtype GameM st eff genv conn a = GameM
  { _unGameM ::
      RWST
        (Env st genv conn)
        [eff]
        ()
        (ExceptT Error STM)
        a
  }
  deriving newtype (Functor, Applicative, Monad)

throw :: Error -> GameM st eff genv conn a
throw = GameM . lift . throwE

throwSoft :: String -> GameM st eff genv conn a
throwSoft = throw . SoftError

throwHard :: String -> GameM st eff genv conn a
throwHard = throw . HardError

liftSTM :: STM a -> GameM st eff genv conn a
liftSTM = GameM . lift . lift

perform :: eff -> GameM st eff genv conn ()
perform eff = GameM $ tell [eff]

getConnection :: GameM st eff genv conn conn
getConnection = connection <$> GameM ask

getState :: GameM st eff genv conn st
getState = liftSTM . STM.readTVar . state =<< GameM ask

setState :: st -> GameM st eff genv conn ()
setState st = do
  tvState <- state <$> GameM ask
  liftSTM $ STM.writeTVar tvState st

modifyState :: (st -> st) -> GameM st eff genv conn ()
modifyState f = do
  tvState <- state <$> GameM ask
  liftSTM $ STM.modifyTVar tvState f

getEnv :: GameM st eff genv conn genv
getEnv = gameEnv <$> GameM ask

runGameM :: Env st genv conn -> (eff -> IO ()) -> GameM st eff genv conn a -> IO (Either Error a)
runGameM env runEffect (GameM game) =
  (STM.atomically $ runExceptT $ evalRWST game env ()) >>= \case
    Left err -> pure (Left err)
    Right (x, effects) -> do
      (traverse_ runEffect effects *> pure (Right x))
        `Exception.catch`
          \e -> pure (Left $ HardError $ show (e :: SomeException))

