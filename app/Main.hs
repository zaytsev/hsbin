module Main (main) where

import Lib
import Network.Wai.Handler.Warp (run)
import Servant (serve)

import qualified DB
import Hasql.Pool (acquire, use)

main :: IO ()
main = do
    settings <- DB.getConnectionSettings
    pool <- acquire 8 (Just 2000) settings
    _ <- use pool DB.migrate
    let env = mkEnv pool
    run 3001 $ serve api $ app env
