{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS -fplugin=MonadicBang #-}

-- | This module makes it possible to run GHC's Parser with plugins on source
-- files, and check what (if any) errors it produced
module MonadicBang.Test.Utils.RunGhcParser where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Foldable

import GHC
import GHC.Driver.Plugins
import GHC.Driver.Env.Types
import GHC.Driver.Config.Finder
import GHC.Driver.Session
import GHC.LanguageExtensions qualified as LangExt
import GHC.Data.EnumSet qualified as ES
import GHC.Data.StringBuffer
import GHC.Settings.IO
import GHC.Types.SourceFile
import GHC.Types.SourceError
import GHC.Unit.Types
import GHC.Unit.Finder
import GHC.Utils.Fingerprint

import GHC.Paths qualified

import MonadicBang qualified

-- | Parses a module
parseGhc :: MonadIO m => String -> m (Either SourceError ParsedModule)
parseGhc src = do
  let dflags = !initialDynFlags
      modNameStr = "MonadicBang.Test.Tmp"
      modName = mkModuleName modNameStr
      modSummary = ModSummary
        { ms_mod = mkModule (stringToUnit modNameStr) modName
        , ms_hsc_src = HsSrcFile
        , ms_location = mkHomeModLocation (initFinderOpts dflags) modName ""
        , ms_hs_hash = fingerprintString src
        , ms_obj_date = Nothing
        , ms_dyn_obj_date = Nothing
        , ms_iface_date = Nothing
        , ms_hie_date = Nothing
        , ms_srcimps = []
        , ms_textual_imps = []
        , ms_ghc_prim_import = False
        , ms_parsed_mod = Nothing
        , ms_hspp_file = modNameStr
        , ms_hspp_opts = dflags
        , ms_hspp_buf = Just $ stringToStringBuffer src
        }
  runDefaultGhc dflags . handleSourceError (pure . Left) $
    Right <$> parseModule modSummary

runDefaultGhc :: MonadIO m => DynFlags -> Ghc a -> m a
runDefaultGhc dflags action = liftIO do
  runGhc (Just GHC.Paths.libdir) (do setSessionDynFlags dflags >> addPlugin >> action)
  where
    addPlugin = do
      let session = !getSession
          plugins = hsc_plugins session
      setSession (session{hsc_plugins = plugins{staticPlugins = StaticPlugin (PluginWithArgs MonadicBang.plugin []) : staticPlugins plugins}})

initialDynFlags :: MonadIO m => m DynFlags
initialDynFlags = do
  dflags <- withExts
  pure $ dflags{generalFlags = ES.insert Opt_ImplicitImportQualified $ generalFlags dflags}
  where
#if MIN_VERSION_ghc(9,6,0)
    withExts = do pure $ foldl' xopt_set (defaultDynFlags !settings') $ exts
#else
    withExts = do pure $ foldl' xopt_set (defaultDynFlags !settings' $ error "llvmConfig") $ exts
#endif
    exts = [LangExt.LambdaCase]

settings' :: MonadIO m => m Settings
settings' = either (error . showSettingsError) id <$> runExceptT (initSettings GHC.Paths.libdir)
  where
    showSettingsError (SettingsError_MissingData s) = s
    showSettingsError (SettingsError_BadData s) = s
