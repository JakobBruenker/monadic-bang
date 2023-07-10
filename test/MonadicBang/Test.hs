{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Prelude hiding ((<>))
#if MIN_VERSION_ghc(9,6,0)
#else
import Control.Applicative (liftA2)
#endif
import Data.Foldable
import Data.Traversable
import System.IO

import MonadicBang.Test.Utils
import MonadicBang.Test.ShouldPass
import MonadicBang.Test.ShouldFail

import GHC.Utils.Outputable
import GHC.Utils.Ppr (Mode(PageMode))
import System.Exit (exitFailure)

main :: IO ()
main = do
  (numFailures, numFailedSuites) <- liftA2 (,) sum (length . filter (> 0)) <$> for suites \suite -> do
    failures <- runSuite suite
    for_ failures \failure -> putSDoc stderr $ vcat [space, prettyFail failure]
    pure $ length failures
  if numFailures == 0
    then putStrLn "All tests passed!"
    else do
      putSDoc stdout $
        space $+$ plural' numFailures "test" <+> text "in" <+> int numFailedSuites <> char '/' <> plural' (length suites) "suite" <+> text "failed."
      exitFailure
  where
    plural' :: Int -> String -> SDoc
    plural' n (text -> s) = int n <+> case n of
      1 -> s
      _ -> s <> char 's'

putSDoc :: Handle -> SDoc -> IO ()
putSDoc = printSDocLn defaultSDocContext (PageMode True)

suites :: [TestType]
suites =
  [ shouldPass
  , shouldFail
  ]
