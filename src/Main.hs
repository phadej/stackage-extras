{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat

import Control.Exception    (IOException, catch)
import Control.Lens
import Control.Monad.Catch  (throwM)
import Data.Aeson.Lens
import Data.Foldable        (traverse_)
import Data.List            (nub, sort)
import Data.Monoid          ((<>))
import Data.Text.Lens       (packed)
import Data.Version         (showVersion)
import Data.Yaml            (Value, decodeFileEither)
import Distribution.Package (PackageName (..))
import System.Exit          (exitFailure)
import System.IO            (hPutStrLn, stderr)

import qualified Distribution.PackDeps as PackDeps

import qualified Options.Applicative as O

data Opts = Opts
    { _optsCmd :: Value -> IO ()
    }

makeLenses ''Opts

execCmd :: Opts -> IO ()
execCmd opts = do
    let path = "build-constraints.yaml"
    let cmd = opts ^. optsCmd
    v' <- decodeFileEither path
    case v' of
        Left err -> throwM err
        Right v  -> cmd v

optsParser :: O.Parser Opts
optsParser = Opts
    <$> cmdParser

cmdParser :: O.Parser (Value -> IO ())
cmdParser = O.subparser $ mconcat
    [ p "packdeps" packdepsCmd "Print outdated package dependencies"
    ]
  where
    p :: String -> (Value -> IO ()) -> String -> O.Mod O.CommandFields (Value -> IO ())
    p cmd commandActions desc =
         O.command cmd $ O.info (O.helper <*> pure commandActions) $ O.progDesc desc

packdepsCmd :: Value -> IO ()
packdepsCmd bc = do
    newest <- catch
        PackDeps.loadNewest
        (\e -> hPutStrLn stderr (failedToLoad e) *> exitFailure)
    traverse_ (f newest) $ nub $ sort pkgs
  where
    f :: PackDeps.Newest -> String -> IO ()
    f newest pkgname = case PackDeps.getPackage pkgname newest of
        Nothing -> pure ()
        Just di -> case PackDeps.checkDeps newest di of
            (_, _, PackDeps.AllNewest)                    -> pure ()
            (pkgName', version, PackDeps.WontAccept ps _) -> do
                putStrLn $ unPackageName pkgName' <> "-" <> showVersion version
                traverse_ g ps

    g :: (String, String) -> IO ()
    g (pkgName, version) =
        putStrLn $ "- " <> pkgName <> "-" <> version

    pkgs :: [String]
    pkgs = bc ^.. key "packages" . members . values . _String . from packed

    failedToLoad :: IOException -> String
    failedToLoad err = unlines
        [ show err
        , "Failed to read package database. Running 'cabal update' might help."
        ]

main :: IO ()
main =
    O.execParser opts >>= execCmd
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "TBD"
        , O.header "stack.yaml modification tools"
        ]
