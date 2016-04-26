import qualified Data.Set as Set
import           Test.QuickCheck

import           Dotfiles
import           Dotfiles.Utils


norm_idemp e s = normalize e s == normalize e (denormalize e s)


filter_valid_idemp dfs = Set.union (filter valid dfs) (filter invalid dfs) == dfs


filter_tracked_idemp dfs = Set.difference v t == Set.union v p
    where t = filter' tracked dfs
          v = filter' valid dfs
          p = filter' pending dfs


main :: IO ()
main = do
  env <- getEnv
  cfg <- readCfg env
  let dfs = mkDotfiles env cfg
      vdfs = filter' valid dfs
      idfs = filter' invalid dfs
      tdfs = filter' tracked dfs
      pdfs = filter' pending dfs
      norm' = norm_idemp env
      in do
         quickCheck norm'
         filter_valid_idemp dfs
         filter_tracked_idemp dfs
         return ()
