module Main where

import Game.Game

import SecretData
import SecretUtility
import Data.Maybe
import Data.List
import System.Random
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Concurrent
import qualified Control.Monad.Parallel as Parr

import Debug.Trace
type CommsList = MVar [(MVar String, String)]
main = do
  commsList <- newMVar []
  initialize [
    GameDescriptor {
      playGame = playSecretHitler commsList,
      descName = descriptorName,
      shardNames = ["HitlerServer","Kittens","Explode","Unicorn"],
      onMessage = hitlerMessage commsList}
    ]

givePlayerRoles :: HitlerState -> IO ()
givePlayerRoles gs = do
  forM_ (players gs) (\p -> flip tellPlayer p $ "Info|You are " ++ (keyFromIdent . plaSecretIdentity $ p))
  if (length . players $ gs) > 6
    then forM_ (filter (hasId $ NotHitler Fascist) $ players gs) tellEverything
    else forM_ (filter ((||) <$> hasId (NotHitler Fascist) <*> hasId Hitler) $ players gs) tellEverything
  where
    keyFromIdent si = case si of
                        NotHitler Liberal -> "liberal"
                        NotHitler Fascist -> "fascist"
                        Hitler -> "hitler"
                        NoIdentity -> error "No Identity midgame rip"
    hasId s = (==s) . plaSecretIdentity
    tellEverything p = forM_ 
      [("Info|The Liberals are ", NotHitler Liberal),
       ("Info|The Fascists are ", NotHitler Fascist),
       ("Info|Hitler is ", Hitler)]
      (\(a,b) -> flip tellPlayer p $ a ++ (intercalate "\n" . map plaName . filter (hasId b) $ players gs))

playSecretHitler :: CommsList -> [Client] -> IO StopCode
playSecretHitler hs clis = 
  if length clis < 5
    then return "Not Enough Players"
    else do 
      consoleLog "Starting Game" 
      plas <- mapM clientToPlayer clis
      -- bootstrapGame's last arg is a random generator
      bootedGame <- freshGame plas <$> newStdGen
      -- Add it to the game list
      modifyMVar_ hs $ return . (map (comm &&& plaName) plas ++)
      givePlayerRoles bootedGame 
      stopcode <- gameLoop bootedGame 
      tellEveryone stopcode bootedGame
      consoleLog $ "Shard " ++ shardName bootedGame ++ " terminated with " ++ show stopcode
      return stopcode

hitlerMessage :: CommsList -> Client -> String -> IO ()
hitlerMessage ks c s = do 
  comms <- readMVar ks
  mapM_ (\x -> when (snd x == cliName c) $ putMVar (fst x) s) comms
  consoleLog $ "Got message: \"" ++ s ++ "\" from client " ++ cliName c

doRound :: HitlerState -> IO HitlerState
doRound game = do
  (game', electionPass) <- electGovernment game
  if electionPass
    then if hitlerElected game'
      then do
        tellEveryone "Info|Hitler was elected chancellor" game'
        return game' {stopGame = Just "Victory Fascist"}
      else legislativeSession game' 
    else if electionTracker game' == 3
      then do
        tellEveryone "Info|The vote failed too many times, so the top policy card was enacted" game'
        flip tellEveryone game' $ case head . deck $ game' of
          Policy Fascist -> "Info|A Fascist Policy was played!"
          Policy Liberal -> "Info|A Liberal Policy was played!"
        return $ (passPolicy game' . head . deck $ game') {electionTracker = 0, deck = tail . deck $ game'}
      else return game' -- Election tracker modified in electGovernment

gameLoop :: HitlerState -> IO String
gameLoop game = do
  newgame <- doRound game 
  maybe (gameLoop newgame) return (stopGame newgame)

electGovernment :: HitlerState -> IO (HitlerState,Bool) -- Bool is True if a new government is in
electGovernment game = do
  -- Select new president from helper function
  let game' = installPresident game
  -- Ask the new president for the plaName of the next chancellor
  newChance <- flip unsafeGetPlayer game' <$> askPlayerUntil (validNextChancellor game') "Chancellor" (president game') 
  tellEveryone ("Info|The proposed government is: " ++ plaName (president game') ++ " as President and " ++ plaName newChance ++ " as Chancellor") game'
  -- Ask each player for their vote until it is valid, and store it in votes
  votes <- map read <$> Parr.mapM (askPlayerUntil isVote "Vote") (players game') 
  tellEveryone (formatVotes votes (players game')) game'
  -- check if the vote passes
  if votePass votes
    -- If it does, put in the new pres and chan and prevGov and return the new state
    then do
      tellEveryone "Info|The vote passed!" game'
      tellEveryone ("Info|The new government is: " ++ plaName (president game') ++ " as President and " ++ plaName newChance ++ " as Chancellor") game'
      return (game' {chancellor = newChance}, True)
    -- Otherwise, advance the election tracker and return false for failure
    else do
      tellEveryone ("Info|The vote failed! The election tracker is now in posisition " ++ (show . (+1) . electionTracker $ game') ++ "!") game'
      return (game' {electionTracker = electionTracker game + 1}, False)

-- Do a legislative session (pass a policy), then return that policy and the modified deck
legislativeSession :: HitlerState -> IO HitlerState
legislativeSession state = do
  -- Ask the president to discard a card until a valid choice is made
  discard <- read <$> 
    askPlayerUntil 
      -- The card is in the top 3 cards
      (isPolicyIn drawn)
      ("Discard|" ++ intercalate "," (map show drawn))
      (president state)
  let cards = delete discard drawn
  -- Ask the chancellor to play a card until a valid choice is made
  playcardr <- 
    askPlayerUntil 
      (\x -> (fascistPolicies state > 4 && x=="Veto") || isPolicyIn cards x) 
      ("Play|" ++ intercalate "," (map show cards)) 
      (chancellor state)
  playcardm <- 
    if playcardr /= "Veto"
      then return . Just $ read playcardr
      else do
        presResp <- askPlayerUntil isVote "Veto" (president state)
        if presResp == "Ja"
          then return Nothing
          else Just . read <$> askPlayerUntil 
                 (isPolicyIn cards) 
                 ("Play|" ++ (intercalate "," . map show $ delete discard drawn))
                 (chancellor state)
  if isJust playcardm
    then do
      let playcard = fromJust playcardm 
      -- Move first three cards of the old deck to the back
      let newdeck = drop 3 $ deck state ++ delete playcard drawn
      --Pass the policy and check victories
      let newstate = applyVictories $ passPolicy (state {deck = newdeck}) playcard
      --Do the special fascist action if a fascist policy is passed
      case playcard of
        Policy Fascist -> do
          tellEveryone "Info|A Fascist Policy was played!" state
          fascistAction newstate 
        Policy Liberal -> do 
          tellEveryone "Info|A Liberal Policy was played!" state
          return newstate
  else do
    tellEveryone "Info|The agenda was vetoed!" state
    return $ state {deck = (drop 3 . deck $ state) ++ drawn}
  where
    drawn = take 3 $ deck state

fascistAction :: HitlerState -> IO HitlerState
fascistAction gs 
  | playerCount gs < 7 = useActionSet [return gs, return gs, peekThree, bullet, bullet, return gs]
  | playerCount gs < 9 = useActionSet [return gs, investigate, specialElection, bullet, bullet, return gs]
  | otherwise = useActionSet [investigate, investigate, specialElection, bullet, bullet, return gs]
  where
   useActionSet xs = xs !! (fascistPolicies gs - 1)
   --President looks at top three cards from deck
   peekThree = do 
     tellPlayer ("Info|The top three cards are: " ++ (show . take 3 . deck $ gs)) (president gs) 
     tellEveryone ("Info|The President (" ++ (plaName.president$gs) ++ ") has peeked at the top three cards!") gs
     return gs
   --President looks at another player's secret identity
   investigate = do
     player <- fromJust . flip getPlayer gs <$> askPlayerUntil (`isPlayerInGame` gs) "Investigate" (president gs)
     tellPlayer ("Info|Player \"" ++ plaName player ++ "\" is " ++ (show . plaSecretIdentity $ player)) (president gs)
     tellEveryone ("Info|The President (" ++(plaName.president$gs)++ ") has Investigated " ++ plaName player ++ "!") gs
     return gs
   --President chooses next canidate
   specialElection = do
     player <- fromJust . flip getPlayer gs <$> askPlayerUntil (`isPlayerInGame` gs) "Special Election" (president gs)
     tellEveryone ("Info|" ++ (plaName . president $ gs) ++ " has called a special election, with " ++ plaName player ++ " as the next presidential candidate") gs
     return $ gs {nextPresident = player}
   --President shoots (removes from game) a player
   bullet = do
     player <- fromJust . flip getPlayer gs <$> askPlayerUntil (`isPlayerInGame` gs) "Kill" (president gs)
     tellEveryone ("Info|" ++ (plaName . president $ gs) ++ " has shot and killed " ++ plaName player) gs
     if plaSecretIdentity player == Hitler
       then do
         tellEveryone "Info|Hitler was killed!" gs
         return $ gs {stopGame = Just "Victory Liberal", players = delete player $ players gs}
       else do
         tellEveryone ("Info|Rest in pieces, " ++ plaName player) gs
         tellEveryone ("Info|Confirmed not Hitler: " ++ plaName player) gs
         tellEveryone ("Disconnect|" ++ plaName player) gs
         if plaName player == (plaName . nextPresident $ gs)
           then return $ gs {nextPresident = findNextPresident gs, players = delete player $ players gs}
         --Return the game without the player in it. RIP player
           else return $ gs {players = delete player $ players gs}
