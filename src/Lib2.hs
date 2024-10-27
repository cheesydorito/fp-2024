{-# LANGUAGE InstanceSigs #-}

module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition,
      executeShowcase,
      main
    ) where


-- | An entity which represents user input.
data Query
  = QueryStore                   -- Query the whole store
  | QueryYear Int                -- Query for a specific year
  | QueryGenre Int String        -- Query for a genre in a specific year
  | QuerySinger Int String String -- Query for a singer in a specific genre/year
  | QueryVinyl Int String String String -- Query for a vinyl by singer/genre/year


instance Eq Query where
  (QueryStore) == (QueryStore) = True
  (QueryYear y1) == (QueryYear y2) = y1 == y2
  (QueryGenre y1 g1) == (QueryGenre y2 g2) = y1 == y2 && g1 == g2
  (QuerySinger y1 g1 s1) == (QuerySinger y2 g2 s2) = y1 == y2 && g1 == g2 && s1 == s2
  (QueryVinyl y1 g1 s1 v1) == (QueryVinyl y2 g2 s2 v2) = y1 == y2 && g1 == g2 && s1 == s2 && v1 == v2
  _ == _ = False

instance Show Query where
  show QueryStore = "Query: Vinyl Store"
  show (QueryYear y) = "Query: Year " ++ show y
  show (QueryGenre y g) = "Query: Year " ++ show y ++ ", Genre " ++ g
  show (QuerySinger y g s) = "Query: Year " ++ show y ++ ", Genre " ++ g ++ ", Singer " ++ s
  show (QueryVinyl y g s v) = "Query: Year " ++ show y ++ ", Genre " ++ g ++ ", Singer " ++ s ++ ", Vinyl " ++ v


-- Parses user's input.
parseQuery :: String -> Either String Query
parseQuery input =
  case words input of
    ["store"] -> Right QueryStore
    [yearStr] -> case reads yearStr of
      [(year, "")] -> Right (QueryYear year)
      _ -> Left "Some error message" 
    [yearStr, genre] -> case reads yearStr of
      [(year, "")] -> Right (QueryGenre year (replaceUnderscores genre))
      _ -> Left "Some error message" 
    [yearStr, genre, singer] -> case reads yearStr of
      [(year, "")] -> Right (QuerySinger year (replaceUnderscores genre) (replaceUnderscores singer))
      _ -> Left "Some error message" 
    [yearStr, genre, singer, vinyl] -> case reads yearStr of
      [(year, "")] -> Right (QueryVinyl year (replaceUnderscores genre) (replaceUnderscores singer) (replaceUnderscores vinyl))
      _ -> Left "Some error message" 
    _ -> Left "" 

replaceUnderscores :: String -> String
replaceUnderscores = map (\c -> if c == '_' then ' ' else c) -- for users to input names with whitespaces 
-- (e.g., "rock_and_roll" becomes "rock and roll")


data ParsingState
  = EmptyState                     -- Initial state, no data yet
  | YearState Int                  -- State where a year has been parsed
  | GenreState Int String          -- State where year and genre have been parsed
  | SingerState Int String String   -- State where year, genre, and singer have been parsed
  | VinylState Int String String String -- Final state with year, genre, singer, and vinyl
  deriving (Show, Eq)


data State = State
  { queries :: [Query]  -- A list of queries to store
  } deriving (Show, Eq)


emptyState :: State
emptyState = State { queries = [] }


addQuery :: Query -> State -> State
addQuery newQuery state = state { queries = newQuery : queries state }

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query =
  case query of
    QueryStore -> 
      let updatedState = addQuery query state
      in Right (Just "You have queried the whole store.", updatedState)

    QueryYear year -> 
      let updatedState = addQuery query state
      in Right (Just $ "You have queried the year: " ++ show year, updatedState)

    QueryGenre year genre -> 
      let updatedState = addQuery query state
      in Right (Just $ "You have queried the genre: " ++ genre ++ " for the year: " ++ show year, updatedState)

    QuerySinger year genre singer -> 
      let updatedState = addQuery query state
      in Right (Just $ "You have queried the singer: " ++ singer ++ " in genre: " ++ genre ++ " for the year: " ++ show year, updatedState)

    QueryVinyl year genre singer vinyl -> 
      let updatedState = addQuery query state
      in Right (Just $ "You have queried the vinyl: " ++ vinyl ++ " by singer: " ++ singer ++ " in genre: " ++ genre ++ " for the year: " ++ show year, updatedState)

executeShowcase :: FilePath -> IO ()
executeShowcase filePath = do
    contents <- readFile filePath
    let commands = lines contents
    mapM_ executeCommand commands

executeCommand :: String -> IO ()
executeCommand command = 
    case parseQuery command of
        Left errorMsg -> putStrLn $ "Error: " ++ errorMsg
        Right query -> putStrLn $ show query

main :: IO ()
main = executeShowcase "/workspaces/fp-2024/src/showcase.txt"