{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module SHsC.Lexer (lexIO, lexIOFile)
where

--import Safe
--import Data.Maybe
--import Data.List (foldl')
import Data.Map(Map)
import Data.Text(Text)
import Control.Monad.Error (MonadError(), throwError)
import qualified Data.Char as C
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO

data Token = IntegerToken Int
           | IfToken
           | ThenToken
           | ElseToken
           | NumberSignToken
           | ColonToken
           | PipeToken
           | UnderscoreToken
           | LessThanToken
           | GreaterThanToken
           | CommaToken
           | SlashToken
           | DotToken
           | BangToken
           | SemicolonToken
           | EqualsToken
           | AmpersandToken
           | ParenOpenToken
           | ParenCloseToken
           | BracesOpenToken
           | BracesCloseToken
           | BracketsOpenToken
           | BracketsCloseToken
           | BackslashToken
           | PlusToken
           | MinusToken
           | AsteriskToken
           | IdToken Text
           | WhiteSpaceToken
           deriving (Show)

data AutomatonRun = AutomatonRun { asState :: AutomatonState
                                 , asString :: Text
                                 } deriving (Show)

data AutomatonState = StateId Int
                    | Error
                    deriving (Eq, Ord, Show)

data Automaton = Automaton { aTransitions :: Map AutomatonState [Char -> AutomatonState]
                           , aFinalStates :: [AutomatonState]
                           , aBuildToken :: Text -> Maybe Token
                           }

catNonErrorStates :: [AutomatonState] -> [AutomatonState]
catNonErrorStates as =
    let isErrState s = case s of { Error -> True; _ -> False }
    in filter (not . isErrState) as

catInFinishStates :: [(Automaton,AutomatonRun)] -> [(Automaton, AutomatonRun)]
catInFinishStates ars =
    let inFinishState :: (Automaton, AutomatonRun) -> Bool
        inFinishState (a, ar) =
            case asState ar of
              Error -> False
              s@(StateId _) -> s `elem` aFinalStates a
    in filter inFinishState ars

_START_STATE_ :: AutomatonState
_START_STATE_ = StateId 0

_START_RUN_ :: AutomatonRun
_START_RUN_ = AutomatonRun _START_STATE_ ""

intFuncFirst :: Char -> AutomatonState
intFuncFirst c = if c `elem` ['1'..'9']
                    then StateId 1
                    else Error
intFuncFollowing :: Char -> AutomatonState
intFuncFollowing c = if c `elem` ['0'..'9']
                        then StateId 1
                        else Error
_INT_AUTOMATON_ :: Automaton
_INT_AUTOMATON_ =
    Automaton { aTransitions = Map.fromList [ (StateId 0, [ intFuncFirst
                                                          , \z -> if z == '0'
                                                                     then StateId 2
                                                                     else Error
                                                          ])
                                            , (StateId 1, [intFuncFollowing])
                                            ]
              , aFinalStates = [StateId 1, StateId 2]
              , aBuildToken = \s -> case TR.decimal s of
                                      Right (n, "") -> Just $ IntegerToken n
                                      _ -> Nothing
              }

idFuncFirst :: Char -> AutomatonState
idFuncFirst c = if c `elem` ['a'..'z'] || c `elem` ['A'..'Z']
                   then StateId 1
                   else Error
idFuncFollowing :: Char -> AutomatonState
idFuncFollowing c = if c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']
                        then StateId 1
                        else Error
_ID_AUTOMATON_ :: Automaton
_ID_AUTOMATON_ =
    Automaton { aTransitions = Map.fromList [ (StateId 0, [idFuncFirst])
                                            , (StateId 1, [idFuncFollowing])
                                            ]
              , aFinalStates = [StateId 1]
              , aBuildToken = Just . IdToken
              }

oneWordAutomaton :: Token -> Text -> Automaton
oneWordAutomaton t s =
    Automaton { aTransitions = Map.fromList $ map genTrans [0..(T.length s)-1]
              , aFinalStates = [StateId $ T.length s]
              , aBuildToken = const $ Just t
              }
    where genTrans :: Int -> (AutomatonState, [Char -> AutomatonState])
          genTrans i = (StateId i, [\e -> if e == s `T.index` i
                                             then StateId (i+1)
                                             else Error])

_WHITE_SPACE_AUTOMATON_ :: Automaton
_WHITE_SPACE_AUTOMATON_ =
    Automaton { aTransitions =
        Map.fromList [ (StateId 0, [\s -> if C.isSpace s
                                             then StateId 1
                                             else Error])
                     , (StateId 1, [\s -> if C.isSpace s
                                             then StateId 1
                                             else Error])
                     ]
              , aFinalStates = [StateId 1]
              , aBuildToken = const $ Just WhiteSpaceToken
              }

stepAutomaton :: Automaton -> AutomatonRun -> Char -> AutomatonRun
stepAutomaton a state input =
    let transFuns = Map.findWithDefault [] -- default: no trans funs
                                        (asState state)
                                        (aTransitions a)
        newStateIds = map ((flip ($)) input) transFuns
        newState sid c = AutomatonRun sid (asString state `T.snoc` c)
    in case catNonErrorStates newStateIds of
         [] -> newState Error input
         sid':[] -> newState sid' input
         _ -> error "non-deterministic automaton"

lexIO :: String -> IO ()
lexIO = lexIO' . T.pack

lexIOFile :: FilePath -> IO ()
lexIOFile f =
    do t <- TIO.readFile f
       lexIO' t

lexIO' :: Text -> IO ()
lexIO' s =
    case runAllAutomatons s of
      Left msg -> putStrLn $ "ERROR: " ++ msg
      Right (token, s') -> do print token
                              if s' == ""
                                 then return ()
                                 else lexIO' s'

runAllAutomatons :: MonadError String m => Text -> m (Token, Text)
runAllAutomatons =
    runAutomatons [ oneWordAutomaton IfToken "if"
                  , oneWordAutomaton ThenToken "then"
                  , oneWordAutomaton ElseToken "else"
                  , oneWordAutomaton PipeToken "|"
                  , oneWordAutomaton NumberSignToken "#"
                  , oneWordAutomaton ColonToken ":"
                  , oneWordAutomaton UnderscoreToken "_"
                  , oneWordAutomaton LessThanToken "<"
                  , oneWordAutomaton GreaterThanToken ">"
                  , oneWordAutomaton EqualsToken "="
                  , oneWordAutomaton AmpersandToken "&"
                  , oneWordAutomaton ParenOpenToken "("
                  , oneWordAutomaton ParenCloseToken ")"
                  , oneWordAutomaton BracesOpenToken "{"
                  , oneWordAutomaton BracesCloseToken "}"
                  , oneWordAutomaton BracketsOpenToken "["
                  , oneWordAutomaton BracketsCloseToken "]"
                  , oneWordAutomaton BackslashToken "\\"
                  , oneWordAutomaton CommaToken ","
                  , oneWordAutomaton SemicolonToken ";"
                  , oneWordAutomaton PlusToken "+"
                  , oneWordAutomaton MinusToken "-"
                  , oneWordAutomaton AsteriskToken "*"
                  , oneWordAutomaton SlashToken "/"
                  , oneWordAutomaton DotToken "."
                  , oneWordAutomaton BangToken "!"
                  , _INT_AUTOMATON_
                  , _ID_AUTOMATON_
                  , _WHITE_SPACE_AUTOMATON_
                  ]

runAutomatons :: MonadError String m
              => [Automaton]
              -> Text
              -> m (Token, Text)
runAutomatons as str = runAutomatons' as str (map (const _START_RUN_) as)

runAutomatons' :: MonadError String m
               => [Automaton]
               -> Text
               -> [AutomatonRun]
               -> m (Token, Text)
runAutomatons' as s ars =
    case T.uncons s of
       Nothing -> buildToken as ars T.empty
       Just (c, s') -> stepAutomatons as ars c s'

buildToken :: MonadError String m
           => [Automaton]
           -> [AutomatonRun]
           -> Text
           -> m (Token, Text)
buildToken as ars s =
    case catInFinishStates (zip as ars) of
      [] -> throwError $ "no automaton in final state: '"
                         ++ T.unpack (T.take 10 $ (T.lines s)!!0) ++ "'"
      (a, AutomatonRun { asState = state, asString = string }):_ ->
          case state of
            Error -> error "the impossible happened (err state)"
            sid@(StateId _) -> if sid `elem` aFinalStates a
                                  then case aBuildToken a string of
                                         Nothing ->
                                             throwError $ "automaton "
                                                          ++ "<some automaton>"
                                                          ++ "inconsistent"
                                         Just t -> return (t, s)
                                  else error $ "the impossible happened "
                                               ++"(non final state)"

stepAutomatons :: MonadError String m
               => [Automaton]
               -> [AutomatonRun]
               -> Char
               -> Text
               -> m (Token, Text)
stepAutomatons as ars input s =
    if isValid'
       then runAutomatons' as s ars'
       else buildToken as ars (input `T.cons` s)
    where stepFuns = map (uncurry stepAutomaton) (zip as ars)
          ars' = map (uncurry ($)) (zip stepFuns [input,input..])
          ass' = map asState ars'
          isValid' = length (catNonErrorStates ass') > 0
