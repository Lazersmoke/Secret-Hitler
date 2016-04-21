module Game.SecretHitler.SecretUtility where

import Game.NetworkedGameEngine
import System.Random
import System.Random.Shuffle
import Control.Concurrent
import Control.Applicative
import Game.SecretHitler.SecretData
import Data.List
import Data.Maybe
-- | Returns True if Hitler is chancellor and there are three fascists policies down
hitlerElected :: HitlerState -> Bool
hitlerElected st = fascistPolicies st >= 3 && (plaSecretIdentity . chancellor $ st) == Hitler
-- | Returns True if The vote should pass (majority Ja)
votePass :: [Vote] -> Bool
votePass vs = length (filter (Ja ==) vs) > length vs `div` 2
-- | Returns a gamestate in which the selected policy has been passed
passPolicy :: HitlerState -> Policy -> HitlerState
passPolicy s p = s {fascistPolicies = fascistPolicies s + change Fascist,
                    liberalPolicies = liberalPolicies s + change Liberal}
               where change team = if p == Policy team then 1 else 0
-- | Get the next player for president. Next in list after president or first player if no president
findNextPresident :: HitlerState -> Player --Finds the next player that should be president
findNextPresident state = if president state `elem` players state
  -- If the president exists, Take the first player after you drop everyone upto and including the president 
  then head . tail $ dropWhile (/=president state) (cycle $ players state) 
  -- If the president is not in the players list, then take the first player instead
  else head . players $ state

-- | Get a player by name from a game state. Nothing indicates player not found.
getPlayer :: String -> HitlerState -> Maybe Player
getPlayer search state = find ((==search) . plaName) (players state)

unsafeGetPlayer :: String -> HitlerState -> Player
unsafeGetPlayer = (fromJust .) . getPlayer 

formatVotes :: [Vote] -> [Player] -> String
formatVotes vs ps = ("Info|The votes were: "++) . concat $ zipWith (\a b -> "\n" ++ show a ++ " " ++ plaName b) vs ps

pWhen :: Eq a => a -> a -> (a -> a) -> a
pWhen input test func = if input == test then func input else input

playerCount :: HitlerState -> Int
playerCount = length . players

validNextChancellor :: HitlerState -> String -> Bool
validNextChancellor game p = (DummyPlayer `elem` previousGovernment game || not (p `isPlayerInPrevGov` game)) && p /= (plaName . president $ game) && p `isPlayerInGame` game

isPlayerInList :: String -> [Player] -> Bool
isPlayerInList s plas = s `elem` map plaName plas

isPlayerInPrevGov :: String -> HitlerState -> Bool
isPlayerInPrevGov = (. previousGovernment) . isPlayerInList 

isPlayerInGame :: String -> HitlerState -> Bool
isPlayerInGame = (. players) . isPlayerInList 

tellEveryone :: String -> HitlerState -> IO ()
tellEveryone s hs = tellClients s . map plaCli $ players hs

tellPlayer :: String -> Player -> IO ()
tellPlayer s p = tellClient s (plaCli p)

clientToPlayer :: Client -> IO Player
clientToPlayer cli = newEmptyMVar >>= \x -> return Player {plaCli = cli, plaSecretIdentity = NoIdentity, plaName = cliName cli, comm = x}

askPlayerUntil :: (String -> Bool) -> String -> Player -> IO String
askPlayerUntil pred s p = do
  tellPlayer ("Ask|" ++ s) p
  readed <- takeMVar (comm p)
  -- Drop 5 for the "Resp|" header
  if length readed > 5 && pred (drop 5 readed)
    then return $ drop 5 readed
    else askPlayerUntil pred s p

applyVictories :: HitlerState -> HitlerState
applyVictories gs 
  | fascistPolicies gs == 6 = gs {stopGame = Just "Victory Fascist"}
  | liberalPolicies gs == 5 = gs {stopGame = Just "Victory Liberal"}
  | otherwise = gs

installPresident :: HitlerState -> HitlerState
installPresident game = game {
  president = nextPresident game,
  nextPresident = findNextPresident game {president = nextPresident game},
  previousGovernment = chancellor game : 
    if playerCount game < 7 
      then [] 
      else [president game]}

freshDeck :: RandomGen gen => gen -> [Policy]
freshDeck = shuffle' (replicate 6 (Policy Liberal) ++ replicate 11 (Policy Fascist)) 17 

isVote :: String -> Bool
isVote = (||) <$> (=="Ja") <*> (=="Nein")

isPolicy :: String -> Bool
isPolicy = (||) <$> (=="Policy Liberal") <*> (=="Policy Fascist")

readElem :: (Eq a, Read a) => [a] -> String -> Bool
readElem = (. read) . flip elem 

isPolicyIn :: [Policy] -> String -> Bool
isPolicyIn ps = (&&) <$> isPolicy <*> readElem ps
--Task list
--  Give Identities
--  Select first president
bootstrapGame :: RandomGen gen => gen -> HitlerState -> HitlerState
bootstrapGame gen gs = gs' 
  where
    gs' = gs {
      nextPresident = findNextPresident gs', 
      players = zipWith (\a b -> b {plaSecretIdentity = a}) shuffledIdMap (players gs)}
    playerNum = playerCount gs
    shuffledIdMap = shuffle' identityMap (length identityMap) gen
    identityMap
      |playerNum < 5 = repeat NoIdentity
      |playerNum < 7 = take playerNum $ [Hitler, NotHitler Fascist] ++ repeat (NotHitler Liberal)
      |playerNum < 9 = take playerNum $ Hitler:replicate 2 (NotHitler Fascist) ++ repeat (NotHitler Liberal)
      |otherwise = take playerNum $ Hitler:replicate 3 (NotHitler Fascist) ++ repeat (NotHitler Liberal)

newGameState :: RandomGen gen => [Player] -> gen -> HitlerState
newGameState plas gen = HitlerState {
  players = plas,
  president = DummyPlayer,
  chancellor = DummyPlayer,
  previousGovernment = [],
  nextPresident = DummyPlayer,
  deck = freshDeck gen,
  liberalPolicies = 0,
  fascistPolicies = 0,
  electionTracker = 0,
  stopGame = Nothing,
  shardName = shardNameList !! fst (randomR (0, length shardNameList - 1) gen)} 

freshGame :: RandomGen gen => [Player] -> gen -> HitlerState
freshGame p g = bootstrapGame g . newGameState p $ g

shardNameList :: [String]
shardNameList = ["Franz Anton Basch","Josef Blosche","Adolf Eichmann","Erhard Heiden","Edmund Heines","Josef Kieffer","Bruno Kitt","Fritz Knoechlein","Paul Nitsche","Friedrich Schubert","Bernhard Siebken","Wilhelm Trapp","Robert Heinrich Wagner"]

