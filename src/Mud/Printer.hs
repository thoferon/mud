module Mud.Printer where

import Control.Monad.Trans
import Control.Monad.Trans.Free

data PrinterF a = PutLine String (() -> a) deriving Functor

type PrinterT = FreeT PrinterF

runPrinterT :: MonadIO m => PrinterT m a -> m a
runPrinterT = iterT interpreter
  where
    interpreter :: MonadIO m => PrinterF (m a) -> m a
    interpreter (PutLine str f) = liftIO (putStrLn str) >>= f

class Monad m => MonadPrinter m where
  putLine :: String -> m ()

instance Monad m => MonadPrinter (PrinterT m) where
  putLine str = liftF $ PutLine str id

instance {-# OVERLAPPABLE #-} (MonadTrans t, MonadPrinter m, Monad (t m))
  => MonadPrinter (t m) where
  putLine = lift . putLine
