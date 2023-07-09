{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
#if MIN_VERSION_ghc(9,6,0)
{-# LANGUAGE ScopedTypeVariables #-}
#endif

module MonadicBang.Test.Utils where

import Control.Monad
import Data.Foldable
import Data.Function

import GHC.Stack

import GHC
import GHC.Driver.Errors.Types
import GHC.Types.Error
import GHC.Types.SourceError
import GHC.Utils.Outputable hiding ((<>))

import MonadicBang.Test.Utils.RunGhcParser

-- TODO: This should use a Writer to collect all errors
type Test = HasCallStack => IO ()

assertEq :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
-- We don't care about seeing where the `error` call itself happens in the
-- call stack, so we freeze it
assertEq expected actual = when (expected /= actual) $ withFrozenCallStack do
  error $ "Expected " <> show expected <> ", but got " <> show actual

sdocEq :: SDoc -> SDoc -> Bool
sdocEq = (==) `on` showSDocUnsafe

assertFailWith :: (HasCallStack, Outputable a) => [PsMessage] -> Either SourceError a -> IO ()
assertFailWith expected = \case
  Right result -> withFrozenCallStack $ error . showSDocUnsafe $
    text "\n    Expected failure with" $$
    diagnosticsSDoc expected $$
    text "    but execution succeeded with this result:" $$
    ppr result
  Left err -> unless sameErrors do
    error . showSDocUnsafe $
      text "\n    Expected failure with" $$
      diagnosticsSDoc expected $$
      text "    but execution failed with these errors instead:" $$
      diagnosticsSDoc errMsgs
    where
      errMsgs = toList (srcErrorMessages err)
      toPsMessage = \case
        GhcPsMessage m -> Just m
        _ -> Nothing
      listEq eq xs ys = and $ zipWith eq xs ys
      sameErrors = maybe False (((listEq . listEq) sdocEq `on` map (unDecorated . diagMsg)) expected) $ traverse toPsMessage errMsgs
  where
    diagnosticsSDoc diags = vcat (map (vcat . unDecorated . diagMsg) diags)

    diagMsg :: forall a . Diagnostic a => a -> DecoratedSDoc
#if MIN_VERSION_ghc(9,6,0)
    diagMsg = diagnosticMessage (defaultDiagnosticOpts @a)
#else
    diagMsg = diagnosticMessage
#endif

assertParseFailWith :: HasCallStack => [PsMessage] -> String -> IO ()
assertParseFailWith expected source = withFrozenCallStack do
  assertFailWith expected . fmap pm_parsed_source =<< parseGhc source
