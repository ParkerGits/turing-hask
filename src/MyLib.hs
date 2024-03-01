{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib (run, runTest) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate, intersperse, uncons, unsnoc)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import GHC.Generics (Generic)
import Options.Applicative (
  Parser,
  argument,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  metavar,
  progDesc,
  str,
  (<**>),
 )

type MachineState = String

data Symbol = Symbol Char | Blank deriving (Generic, Eq, Ord)

instance Show Symbol where
  show (Symbol c) = [c]
  show Blank = "_"

data Direction = L | R

type Alphabet = [Symbol]

data MachineInput = MachineInput
  { start :: MachineState
  , accept :: MachineState
  , reject :: MachineState
  , delta :: [DeltaInput]
  }
  deriving (Generic, Show)

instance ToJSON MachineInput
instance FromJSON MachineInput

data DeltaInput = DeltaInput
  { from :: MachineState
  , to :: [TransitionInput]
  }
  deriving (Generic, Show)

instance ToJSON DeltaInput
instance FromJSON DeltaInput

data TransitionInput = TransitionInput
  { result :: [String]
  , on :: Char
  }
  deriving (Generic, Show)

instance ToJSON TransitionInput
instance FromJSON TransitionInput

data TuringMachine = TuringMachine
  { transition :: MachineState -> Symbol -> Eval (MachineState, Symbol, Direction)
  , startState :: MachineState
  , acceptState :: MachineState
  , rejectState :: MachineState
  }

data Configuration = Configuration
  { left :: [Symbol]
  , headState :: MachineState
  , right :: [Symbol]
  }

instance Show Configuration where
  show config =
    intercalate
      ""
      [ join $ map show $ left config
      , ("(q" <> (headState config) <> ")")
      , join $ map show $ right config
      ]

data Result = Accepts | Rejects | Processing

newtype Eval a = Eval {unEval :: ExceptT String (WriterT [Configuration] IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError String
    , MonadWriter [Configuration]
    , MonadIO
    )

run :: IO ()
run = do
  inputString <- input <$> parseOpts
  inputJSON <- BL.getContents
  case eitherDecode inputJSON :: Either String MachineInput of
    Left err -> putStrLn err
    Right m -> runMachine (machineFromJSON m) inputString >>= printResults

recognize :: TuringMachine -> String -> Eval Bool
recognize m s = initConfig m s >>= compute m

initConfig :: TuringMachine -> String -> Eval Configuration
initConfig m s =
  let config' = Configuration [] (startState m) (map Symbol s)
   in newConfig config'

compute :: TuringMachine -> Configuration -> Eval Bool
compute m config =
  do
    config' <- yield m config
    let headState' = headState config'
     in case stateResult m headState' of
          Accepts -> return True
          Rejects -> return False
          Processing -> compute m config'

yield :: TuringMachine -> Configuration -> Eval Configuration
yield m config =
  let b = fromMaybe Blank $ (fst <$> (uncons $ right config))
      q = headState config
   in do
        (q', b', direction) <- transition m q b
        case direction of
          L -> moveLeft q' b' config
          R -> moveRight q' b' config

moveLeft :: MachineState -> Symbol -> Configuration -> Eval Configuration
moveLeft q b config =
  case unsnoc $ left config of
    -- trying to move off the tape
    Nothing -> case uncons $ right config of
      -- there is no tape
      Nothing -> throwError "Error: empty tape"
      -- at the start of the tape
      Just (_, rs) ->
        let config' = Configuration [] q (b : rs)
         in newConfig config'
    -- can move left
    Just (ls, l) -> case uncons $ right config of
      -- at the righthand end of the tape
      Nothing ->
        let config' = Configuration ls q [l, b]
         in newConfig config'
      -- in the middle of the tape
      Just (_, rs) ->
        let config' = Configuration ls q (l : b : rs)
         in newConfig config'

moveRight :: MachineState -> Symbol -> Configuration -> Eval Configuration
moveRight q b config = case uncons $ right config of
  -- at the righthand end of the tape
  Nothing ->
    let config' = Configuration (left config ++ [b]) q []
     in newConfig config'
  -- in the middle of the tape
  Just (_, rs) ->
    let config' = Configuration (left config ++ [b]) q rs
     in newConfig config'

newConfig :: Configuration -> Eval Configuration
newConfig config = tell [config] >> return config

stateResult :: TuringMachine -> MachineState -> Result
stateResult m s
  | acceptState m == s = Accepts
  | rejectState m == s = Rejects
  | otherwise = Processing

nothingToBlank :: Maybe Symbol -> Symbol
nothingToBlank (Just s) = s
nothingToBlank Nothing = Blank

printResults :: (Either String Bool, [Configuration]) -> IO ()
printResults (results, configurations) = do
  mapM_ (putStrLn . show) $ configurations
  case results of
    Left error -> putStrLn error
    Right accepts -> if accepts then putStrLn "Accepts!" else putStrLn "Rejects."

machineFromJSON :: MachineInput -> TuringMachine
machineFromJSON m =
  TuringMachine
    { transition = transitionFromJSON $ delta m
    , startState = start m
    , acceptState = accept m
    , rejectState = reject m
    }

transitionFromJSON ::
  [DeltaInput] ->
  (MachineState -> Symbol -> Eval (MachineState, Symbol, Direction))
transitionFromJSON deltas = \q b -> do
  map <- constructFromToMap deltas
  case Map.lookup q map of
    Nothing -> throwError $ "Unrecognized state: " <> q
    Just transitions -> case Map.lookup b transitions of
      Nothing ->
        throwError $
          "Invalid transition " <> show b <> " from state " <> q <> ". Rejects."
      Just result -> return result
 where
  constructFromToMap deltas =
    Map.fromList
      <$> ( sequence $
              map
                ( \delta -> do
                    toResult <- constructToResultMap $ to delta
                    return (from delta, toResult)
                )
                deltas
          )
  constructToResultMap transitions =
    Map.fromList
      <$> ( sequence $
              map
                ( \to -> do
                    res <- parseResult $ result to
                    return (charToSymbol $ on to, res)
                )
                transitions
          )

charToSymbol :: Char -> Symbol
charToSymbol '_' = Blank
charToSymbol c = Symbol c

parseResult :: [String] -> Eval (MachineState, Symbol, Direction)
parseResult [q, "_", direction] = case direction of
  "L" -> return (q, Blank, L)
  "R" -> return (q, Blank, R)
  d -> throwError $ "Unrecognized direction: " <> d
parseResult [q, b, direction] = case uncons b of
  Nothing -> throwError $ "Empty symbol in transition from state " <> q <> "."
  Just (c, cs) ->
    if not $ null cs
      then throwError $ "Input symbols should each be one character: " <> (c : cs)
      else case direction of
        "L" -> return (q, Symbol c, L)
        "R" -> return (q, Symbol c, R)
        d -> throwError $ "Unrecognized direction: " <> d
parseResult s =
  throwError $
    "Result values should be 3-tuples of [State, Symbol, Direction]. Received: "
      <> (join ["[", intercalate "," s, "]"])

runMachine ::
  TuringMachine -> String -> IO (Either String Bool, [Configuration])
runMachine m input = runWriterT $ runExceptT (unEval $ recognize m input)

testTransition ::
  MachineState -> Symbol -> Eval (MachineState, Symbol, Direction)
testTransition "1" Blank = return ("reject", Blank, R)
testTransition "1" (Symbol 'x') = return ("reject", Symbol 'x', R)
testTransition "1" (Symbol '0') = return ("2", Blank, R)
testTransition "2" (Symbol 'x') = return ("2", Symbol 'x', R)
testTransition "2" Blank = return ("accept", Blank, R)
testTransition "2" (Symbol '0') = return ("3", Symbol 'x', R)
testTransition "3" (Symbol '0') = return ("4", Symbol '0', R)
testTransition "3" (Symbol 'x') = return ("3", Symbol 'x', R)
testTransition "3" Blank = return ("5", Blank, L)
testTransition "4" Blank = return ("reject", Blank, R)
testTransition "4" (Symbol 'x') = return ("4", Symbol 'x', R)
testTransition "4" (Symbol '0') = return ("3", Symbol 'x', R)
testTransition "5" (Symbol '0') = return ("5", Symbol '0', L)
testTransition "5" (Symbol 'x') = return ("5", Symbol 'x', L)
testTransition "5" Blank = return ("2", Blank, R)

testMachine =
  TuringMachine
    { transition = testTransition
    , startState = "1"
    , acceptState = "accept"
    , rejectState = "reject"
    }

runTest :: IO ()
runTest = runMachine testMachine "0000" >>= printResults

data Options = Options
  { input :: String
  }

opts :: Parser Options
opts =
  Options
    <$> argument str (metavar "<machine input>" <> help "Turing Machine Input")

parseOpts :: IO Options
parseOpts =
  execParser $
    info
      (opts <**> helper)
      ( fullDesc
          <> header "Turing Machine Computation Simulator"
          <> progDesc
            "Examine the steps performed by a Turing Machine given an input string."
      )
