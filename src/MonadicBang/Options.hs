{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module MonadicBang.Options where

import Control.Exception
import Control.Algebra
import Control.Carrier.State.Strict
import Control.Effect.Throw
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.List (intercalate, partition)

import GHC
import GHC.Plugins

data Verbosity = DumpTransformed | Quiet

data PreserveErrors = Preserve | Don'tPreserve

data Options = MkOptions {verbosity :: Verbosity, preserveErrors :: PreserveErrors}

parseOptions :: Has (Throw ErrorCall) sig m => Located HsModule -> [CommandLineOption] -> m Options
parseOptions mod' cmdLineOpts = do
  (remaining, options) <- runState cmdLineOpts do
    verbosity <- bool Quiet DumpTransformed <$> extractOpts verboseOpts
    preserveErrors <- bool Don'tPreserve Preserve <$> extractOpts preserveErrorsOpts
    pure $ MkOptions verbosity preserveErrors
  unless (null remaining) . throwError . ErrorCall $
    "Incorrect command line options for plugin MonadicBang, encountered in " ++ modName ++ modFile ++
    "\n\tOptions that were supplied (via -fplugin-opt) are: " ++ intercalate ", " (map show cmdLineOpts) ++
    "\n\tUnrecognized options: " ++ showOpts remaining ++
    "\n\n\tUsage: [-ddump] [-preserve-errors]" ++
    "\n" ++
    "\n\t\t-ddump            Print the altered AST" ++
    "\n\t\t-preserve-errors  Keep parse errors about ! outside of 'do' in their original form, rather then a more relevant explanation." ++
    "\n\t\t                  This is mainly useful if another plugin expects those errors."
  pure options

  where
    verboseOpts = ["-ddump"]
    preserveErrorsOpts = ["-preserve-errors"]
    extractOpts opt = do
      (isOpt, opts') <- gets $ first (not . null) . partition (`elem` opt)
      put opts'
      pure isOpt

    showOpts = intercalate ", " . map show

    modFile = maybe "" ((" in file " ++) . unpackFS . srcSpanFile) $ toRealSrcSpan (getLoc mod')
    modName = maybe "an unnamed module" (("module " ++) . moduleNameString . unLoc) $ (unLoc mod').hsmodName
    toRealSrcSpan = \cases
      (RealSrcSpan rss _) -> Just rss
      (UnhelpfulSpan _) -> Nothing
