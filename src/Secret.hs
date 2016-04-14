{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Network.WebSockets as WS
import Control.Concurrent
import SecretNetworking
import SecretUtility
import SecretData
import System.Random
import qualified Control.Monad.Parallel as Parr

-- Control flow impure code ---------------------------------------------------
main :: IO ()
main = do
  state <- newMVar ([],[])
  --We really don't need the thread ids; just let them die with the app
  _ <- forkIO $ mainLoop state
  _ <- forkIO $ chatLoop state
  WS.runServer "0.0.0.0" 9160 $ connHandler state

chatLoop :: MVar ServerState -> IO ()
chatLoop server = forever $ do
  state <- readMVar server
  let clients = snd state
  --Grab chat messages
  let messages = mapMaybe (`getFromClient` "Chat|") clients
  mapM_ ((`overClientsOf` server) . tellClients) messages
  --Remove the sent chat messages from the comms list
  map (\x -> x {cliComms = filter (not . isPrefixOf "Chat|") $ cliComms x}) `overClientsOf` server
  threadDelay 10000 
  
mainLoop :: MVar ServerState -> IO ()
mainLoop server = forever $ do 
  filter isNotEmptyGame `overGamesOf` server
  map markGameReady `overGamesOf` server
  state <- readMVar server
  maybe (threadDelay 100000) (`playGame` server) $ find ready (gamesOf state)

overGamesOf :: ([GameState] -> [GameState]) -> MVar ServerState -> IO ()
overGamesOf func server = modifyMVar_ server $ \s -> return (func (fst s), snd s)

overClientsOf :: ([Client] -> [Client]) -> MVar ServerState -> IO ()
overClientsOf func server = modifyMVar_ server $ \s -> return (fst s, func (snd s))

givePlayerRoles :: GameState -> IO ()
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

playGame :: GameState -> MVar ServerState -> IO ()
playGame game server = do
  map (\gs -> pWhen gs game markAllUnready) `overGamesOf` server
  --Spawn new thread to deal with each game
  _ <- forkIO $ do
    consoleLog $ "Starting game on shard: " ++ shardName game
    -- bootstrapGame's last arg is a random generator
    bootedGame <- bootstrapGame game <$> newStdGen
    givePlayerRoles bootedGame 
    stopcode <- gameLoop bootedGame server 
    flip tellEveryone bootedGame $ case stopcode of
      Victory Fascist -> "Info|FASCISTS WIN!!!" 
      Victory Liberal -> "Info|LIBERALS WIN!!!" 
    -- ServerState updated upon return of gameLoop
    delete bootedGame `overGamesOf` server
    consoleLog $ "Shard " ++ shardName bootedGame ++ " terminated with " ++ show stopcode
  --Must match return type of threadDelay
  return ()

doRound :: GameState -> MVar ServerState -> IO GameState
doRound game server = do
  (game', electionPass) <- electGovernment game server
  if electionPass
    then if hitlerElected game'
      then do
        tellEveryone "Info|Hitler was elected chancellor" game'
        return game' {stopGame = Just $ Victory Fascist}
      else legislativeSession game' server
    else if electionTracker game' == 3
      then do
        tellEveryone "Info|The vote failed too many times, so the top policy card was enacted" game'
        flip tellEveryone game' $ case head . deck $ game' of
          Policy Fascist -> "Info|A Fascist Policy was played!"
          Policy Liberal -> "Info|A Liberal Policy was played!"
        return $ (passPolicy game' . head . deck $ game') {electionTracker = 0, deck = tail . deck $ game'}
      else return game' -- Election tracker modified in electGovernment

gameLoop :: GameState -> MVar ServerState -> IO StopCode
gameLoop game server = do
  writeState game server
  newgame <- doRound game server
  writeState newgame server
  maybe (gameLoop newgame server) return (stopGame newgame)

electGovernment :: GameState -> MVar ServerState -> IO (GameState,Bool) -- Bool is True if a new government is in
electGovernment game server = do
  -- Select new president from helper function
  let game' = installPresident game
  -- Ask the new president for the name of the next chancellor
  newChance <- flip unsafeGetPlayer game' <$> askPlayerUntil (validNextChancellor game') "Chancellor" server newPres 
  tellEveryone ("Info|The proposed government is: " ++ plaName newPres ++ " as President and " ++ plaName newChance ++ " as Chancellor") game'
  -- Ask each player for their vote until it is valid, and store it in votes
  votes <- map read <$> Parr.mapM (askPlayerUntil isVote "Vote" server) (players game') 
  tellEveryone (formatVotes votes playerList) game'
  -- check if the vote passes
  if votePass votes
    -- If it does, put in the new pres and chan and prevGov and return the new state
    then do
      tellEveryone "Info|The vote passed!" game'
      tellEveryone ("Info|The new government is: " ++ plaName newPres ++ " as President and " ++ plaName newChance ++ " as Chancellor") game'
      return (game' {chancellor = newChance}, True)
    -- Otherwise, advance the election tracker and return false for failure
    else do
      tellEveryone ("Info|The vote failed! The election tracker is now in posisition " ++ (show . (+1) . electionTracker $ game') ++ "!") game'
      return (game' {electionTracker = electionTracker game + 1}, False)

-- Do a legislative session (pass a policy), then return that policy and the modified deck
legislativeSession :: GameState -> MVar ServerState -> IO GameState
legislativeSession state server = do
  -- Ask the president to discard a card until a valid choice is made
  discard <- read <$> 
    askPlayerUntil 
      -- The card is in the top 3 cards
      (isPolicyIn drawn)
      ("Discard|" ++ intercalate "," (map show drawn))
      server (president state)
  let cards = delete discard drawn
  -- Ask the chancellor to play a card until a valid choice is made
  playcardr <- 
    askPlayerUntil 
      (\x -> (fascistPolicies state > 4 && x=="Veto") || isPolicyIn cards x) 
      ("Play|" ++ intercalate "," (map show cards)) 
      server (chancellor state)
  playcardm <- 
    if playcardr /= "Veto"
      then return . Just $ read playcardr
      else do
        presResp <- askPlayerUntil isVote "Veto" server (president state)
        if presResp == "Ja"
          then return Nothing
          else Just . read <$> askPlayerUntil 
                 (isPolicyIn cards) 
                 ("Play|" ++ (intercalate "," . map show $ delete discard drawn))
                 server (chancellor state)
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
          fascistAction newstate server
        Policy Liberal -> do 
          tellEveryone "Info|A Liberal Policy was played!" state
          return newstate
  else do
    tellEveryone "Info|The agenda was vetoed!" state
    return $ state {deck = (drop 3 . deck $ state) ++ drawn}
  where
    drawn = take 3 $ deck state

fascistAction :: GameState -> MVar ServerState -> IO GameState
fascistAction gs server 
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
     player <- fromJust . flip getPlayer gs <$> askPlayerUntil (`isPlayerInGame` gs) "Investigate" server (president gs)
     tellPlayer ("Info|Player \"" ++ plaName player ++ "\" is " ++ (show . plaSecretIdentity $ player)) (president gs)
     tellEveryone ("Info|The President (" ++(plaName.president$gs)++ ") has Investigated " ++ plaName player ++ "!") gs
     return gs
   --President chooses next canidate
   specialElection = do
     player <- fromJust . flip getPlayer gs <$> askPlayerUntil (`isPlayerInGame` gs) "Special Election" server (president gs)
     tellEveryone ("Info|" ++ (plaName . president $ gs) ++ " has called a special election, with " ++ plaName player ++ " as the next presidential candidate") gs
     return $ gs {nextPresident = player}
   --President shoots (removes from game) a player
   bullet = do
     player <- fromJust . flip getPlayer gs <$> askPlayerUntil (`isPlayerInGame` gs) "Kill" server (president gs)
     tellEveryone ("Info|" ++ (plaName . president $ gs) ++ " has shot and killed " ++ plaName player) gs
     if plaSecretIdentity player == Hitler
       then do
         tellEveryone "Info|Hitler was killed!" gs
         return $ gs {stopGame = Just $ Victory Liberal, players = delete player $ players gs}
       else do
         tellEveryone ("Info|Rest in pieces, " ++ plaName player) gs
         tellEveryone ("Info|Confirmed not Hitler: " ++ plaName player) gs
         tellEveryone ("Disconnect|" ++ plaName player) gs
         if plaName player == (plaName . nextPresident $ gs)
           then return $ gs {nextPresident = findNextPresident gs, players = delete player $ players gs}
         --Return the game without the player in it. RIP player
           else return $ gs {players = delete player $ players gs}

writeState :: GameState -> MVar ServerState -> IO ()
writeState gs = overGamesOf $ (gs :) . filter ((/=shardName gs) . shardName) 
