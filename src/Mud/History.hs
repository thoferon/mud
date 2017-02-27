module Mud.History where

import Prelude hiding (showString, readFile, writeFile)

import Control.Monad.Except
import Control.Monad.Trans.Free

import Data.List
import Data.Time hiding (parseTime)

import Text.Parsec
import Text.Parsec.String

import System.FilePath

import Mud.Error
import Mud.FileSystem

data History = History
  { histLimit   :: Maybe Int
  , histEntries :: [HistoryEntry]
  } deriving (Show, Eq)

defaultHistory :: History
defaultHistory = History
  { histLimit   = Nothing
  , histEntries = []
  }

data HistoryEntry
  = HistDeploy   String UTCTime String [(String, String)]
  | HistUndeploy String UTCTime String
  | HistRollback String UTCTime
  deriving (Show, Eq)

historyEntryProject :: HistoryEntry -> String
historyEntryProject = \case
  HistDeploy n _ _ _ -> n
  HistUndeploy n _ _ -> n
  HistRollback n _   -> n

historyToString :: History -> String
historyToString History{..} =
    maybe "" (\n -> "limit=" ++ show n ++ "\n") histLimit
    ++ intercalate "\n" (map showEntry histEntries)

  where
    showEntry = \case
      HistDeploy projectName time version vars ->
        showTime time ++ " deploy(" ++ showString projectName ++ ", "
        ++ showString version ++ showVars vars ++ ")"
      HistUndeploy projectName time version ->
        showTime time ++ " undeploy(" ++ showString projectName ++ ", "
        ++ showString version ++ ")"
      HistRollback projectName time ->
        showTime time ++ " rollback(" ++ showString projectName ++ ")"

    showTime :: UTCTime -> String
    showTime = formatTime defaultTimeLocale "%F %T"

    showVars :: [(String, String)] -> String
    showVars = mconcat . map ((", " ++) . showVar)

    showVar :: (String, String) -> String
    showVar (k, v) = showString k ++ "=" ++ showString v

    showString :: String -> String
    showString str =
      let step '\\' acc = '\\' : '\\' : acc
          step '\"' acc = '\\' : '\"' : acc
          step '\n' acc = '\\' : 'n'  : acc
          step c acc = c : acc
      in "\"" ++ foldr step "" str ++ "\""

stringToHistory :: FilePath -> String -> Either String History
stringToHistory source = either (Left . show) Right . parse parser source
  where
    parser :: Parser History
    parser = do
      mLimit  <- optionMaybe $ try limit
      entries <- entry `sepBy` (char '\n')
      optional $ char '\n'
      eof
      return History
        { histLimit   = mLimit
        , histEntries = entries
        }

    limit :: Parser Int
    limit = between (string "limit=") (char '\n') $
      read <$> many1 digit

    entry :: Parser HistoryEntry
    entry = do
      t <- time
      _ <- space
      deployEntry t <|> undeployEntry t <|> rollbackEntry t

    time :: Parser UTCTime
    time = do
      str <- count (length "YYYY-mm-dd HH:MM:SS") anyToken
      either fail return $ parseTimeM False defaultTimeLocale "%F %T" str

    deployEntry :: UTCTime -> Parser HistoryEntry
    deployEntry t = between (string "deploy(") (char ')') $ do
      projectName <- escapedString
      commaSep
      version <- escapedString
      vars <- many (commaSep >> variable)
      return $ HistDeploy projectName t version vars

    undeployEntry :: UTCTime -> Parser HistoryEntry
    undeployEntry t = between (string "undeploy(") (char ')') $ do
      projectName <- escapedString
      commaSep
      version <- escapedString
      return $ HistUndeploy projectName t version

    rollbackEntry :: UTCTime -> Parser HistoryEntry
    rollbackEntry t = between (string "rollback(") (char ')') $ do
      projectName <- escapedString
      return $ HistRollback projectName t

    commaSep :: Parser ()
    commaSep = char ',' >> optional (char ' ')

    variable :: Parser (String, String)
    variable = do
      k <- escapedString
      _ <- char '='
      v <- escapedString
      return (k, v)

    escapedString :: Parser String
    escapedString = do
      let specialChar = do
            _ <- char '\\'
            c <- anyToken
            case c of
              '\\' -> return c
              '\"' -> return c
              'n'  -> return '\n'
              _ -> fail $ "unreable special character '\\" ++ c : "'"
      char '"' >> manyTill (specialChar <|> anyToken) (char '"')

data HistoryF a
  = ReadHistory FilePath (History -> a)
  | WriteHistory FilePath History (() -> a)
  deriving Functor

type HistoryT = FreeT HistoryF

runHistoryT :: MonadError MudError m => HistoryT m a -> FileSystemT m a
runHistoryT = iterTM interpreter
  where
    interpreter :: MonadError MudError m => HistoryF (FileSystemT m a)
                -> FileSystemT m a
    interpreter = \case
      ReadHistory  path      f -> actualReadHistory  path      >>= f
      WriteHistory path hist f -> actualWriteHistory path hist >>= f

class Monad m => MonadHistory m where
  readHistory  :: FilePath -> m History
  writeHistory :: FilePath -> History -> m ()

instance Monad m => MonadHistory (HistoryT m) where
  readHistory  path      = liftF $ ReadHistory  path      id
  writeHistory path hist = liftF $ WriteHistory path hist id

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadHistory m, Monad (t m))
  => MonadHistory (t m) where
  readHistory            = lift . readHistory
  writeHistory path hist = lift $ writeHistory path hist

addToHistory :: MonadHistory m => FilePath -> HistoryEntry -> m ()
addToHistory path entry = do
  history <- readHistory path
  let entries  = histEntries history ++ [entry]
      entries' = maybe id takeLast (histLimit history) entries
      history' = history { histEntries = entries' }
  writeHistory path history'

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

actualReadHistory :: (MonadFileSystem m, MonadError MudError m) => FilePath
                  -> m History
actualReadHistory dir = do
  let path = dir </> ".mud-history"
  check <- doesFileExist path
  if check
    then do
      str <- readFile path
      case stringToHistory path str of
        Left  err  -> throwError $ MudErrorUnreadableHistory err
        Right hist -> return hist
    else return defaultHistory

actualWriteHistory :: (MonadFileSystem m, MonadError MudError m) => FilePath
                   -> History -> m ()
actualWriteHistory dir hist = do
  let path  = dir </> ".mud-history"
  writeFile path $ historyToString hist
