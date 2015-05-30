{-# LANGUAGE RecordWildCards #-}

import Data.List (isPrefixOf)
import           Control.Monad
import           Data.Maybe
import qualified Distribution.InstalledPackageInfo  as IPI
import qualified Distribution.ModuleName            as MN
import           Distribution.PackageDescription    as PD
import           Distribution.Simple
import           Distribution.Simple.BuildPaths
import           Distribution.Simple.LocalBuildInfo
import qualified Distribution.Simple.PackageIndex   as PI
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils          (createDirectoryIfMissingVerbose, die,
                                                     info, notice, warn, rewriteFile, cabalVersion)
import           Distribution.Text                  (display)
import           System.FilePath                    (takeDirectory, (<.>),
                                                   (</>))
import           Data.Version

main :: IO ()
main = defaultMainWithHooks
         simpleUserHooks { confHook = confHook',  buildHook = buildHook' }

confHook' :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
confHook' gpd_hbi cflags = do
    unless (versionBranch cabalVersion < [1,21]) $
        die ("base-noprelude cannot be built with Cabal versions newer than 1.20 (`Setup.hs` imported Cabal library version " ++ showVersion cabalVersion ++ ")")

    lbi <- confHook simpleUserHooks gpd_hbi cflags

    let pkgver = pkgVersion . PD.package . localPkgDescr $ lbi

    let (baseVer,baseMods0) = case PI.lookupPackageName (installedPkgs lbi) (PackageName "base") of
                     [(ver', [IPI.InstalledPackageInfo {..}])] -> (ver',map stringlyCoerce exposedModules)

        -- filter out Prelude module from `base`'s exposed module list
        baseMods = filter (/= MN.fromString "Prelude") baseMods0

    unless (versionBranch baseVer `isPrefixOf` versionBranch pkgver) $
        die ("version doesn't match base package version" ++ show (pkgver,baseVer))

    return $! (mapLocalPkgDescr . mapLibrary . fmap . mapExposedModules) (const baseMods) lbi
  where
    -- needed to make compile w/ Cabal >= 1.22
    stringlyCoerce = MN.fromString . display

    -- pseudo-functors
    mapLocalPkgDescr  f lbi = lbi { localPkgDescr  = f (localPkgDescr lbi) }
    mapLibrary        f pd  = pd  { library        = f (library       pd)  }
    mapExposedModules f l   = l   { exposedModules = f (exposedModules l)  }

    verbosity = fromFlag $ configVerbosity cflags

-- buildHook' :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildHook' pd lbi uh bflags = do
    notice verbosity "creating wrapper modules..."

    forM_ baseMods $ \m -> do
        info verbosity ("generating wrapper module for " ++ display m)
        createDirectoryIfMissingVerbose verbosity True (takeDirectory $ apath m)
        rewriteFile (apath m) (genWrapper m)

    buildHook simpleUserHooks pd lbi uh bflags
  where
    genWrapper mn = unlines
                    [ "module " ++ display mn ++ "(module M) where"
                    , "import \"base\" " ++ display mn ++ " as M"
                    ]

    apath mn = autogenModulesDir lbi </> MN.toFilePath mn <.> "hs"

    baseMods = (exposedModules . fromJust . library) pd
    verbosity = fromFlag $ buildVerbosity bflags

-- postBuild' :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postBuild' args bflags pd lbi = do
    putStrLn "postBuild"
    postBuild simpleUserHooks args bflags pd lbi
