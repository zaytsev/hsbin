module Main (main) where

import Lib
import Network.Wai.Handler.Warp (run)
import Servant (serve)

main :: IO ()
main = do
    env <- mkEnv
    run 3001 $ serve api $ app env
