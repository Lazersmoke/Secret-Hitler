module Game.SecretHitler.SecretData where

import Game.Game

import Network.WebSockets as WS
import Control.Monad
import Control.Concurrent

data Identity = Hitler | NotHitler Team | NoIdentity deriving (Show,Eq,Read)
data Player = Player {plaSecretIdentity :: Identity, plaName :: String, plaCli :: Client, comm :: MVar String} | DummyPlayer
data Policy = Policy Team deriving (Show,Eq,Read)
data Vote = Ja | Nein deriving (Show,Eq,Read)
data Team = Liberal | Fascist deriving (Show,Eq,Read)

instance Show Player where
  show DummyPlayer = "<Dummy Player>"
  show pla = "Player: " ++ plaName pla ++ "/" ++ (show . plaSecretIdentity $ pla) 

instance Eq Player where
  (==) DummyPlayer DummyPlayer = True
  (==) DummyPlayer _ = False
  (==) _ DummyPlayer = False
  (==) a b = plaName a == plaName b
{-
Communication model:
  On Client:
    - send handshake with connection info and player name
    - server sends session list and confirm
    - client chooses session, then blocks until a message is sent
  On Server:
    - Wait for client handshakes
    - maintain session list
    - run game when all players ready
-}
data HitlerState = HitlerState {
  players :: [Player],
  president :: Player,
  chancellor :: Player,
  previousGovernment :: [Player],
  nextPresident :: Player,
  deck :: [Policy],
  liberalPolicies :: Int,
  fascistPolicies :: Int,
  electionTracker :: Int,
  stopGame :: Maybe String,
  shardName :: String} deriving (Eq,Show)

debug :: Bool
debug = True

descriptorName :: String
descriptorName = "SecretHitler"

debugLog :: String -> IO ()
debugLog s = when debug $ putStrLn s

debugLogClient :: String -> String -> IO ()
debugLogClient clientName s = when debug $ putStrLn $ "<" ++ clientName ++ "> " ++ s

consoleLog :: String -> IO ()
consoleLog = putStrLn . (("[" ++ descriptorName ++ "]") ++)
