#!/usr/bin/env stack
-- stack --resolver lts-11.8 script
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleInstances #-}  # for instance FromJSON [PackageConfig]

import GHC.Generics
-- import Data.Map (Map)
-- import qualified Data.Map as M
-- import Data.Aeson
-- import Data.Aeson.Types
import qualified Data.Text as T
import Data.Yaml


data PackageConfig = PackageConfig { group :: T.Text
                                   , action :: T.Text
                                   , packages :: [T.Text]
                                   } deriving (Show, Generic)
instance FromJSON PackageConfig


filterAction :: T.Text -> [PackageConfig] -> [PackageConfig]
filterAction actionName packageConfigs = [pc | pc <- packageConfigs
                                             , action pc == actionName]

filterInstall :: [PackageConfig] -> [PackageConfig]
filterInstall = filterAction "install"

filterPurge :: [PackageConfig] -> [PackageConfig]
filterPurge = filterAction "purge"


allPackages :: [PackageConfig] -> [T.Text]
allPackages = concat . map packages

allPackagesText :: [PackageConfig] -> T.Text
allPackagesText = T.intercalate " " . allPackages


main :: IO ()
main = do
  let filepath = "apt_packages.yaml"
  file <- decodeFileEither filepath :: IO (Either ParseException [PackageConfig])
  putStr . either show T.unpack $ allPackagesText . filterInstall <$> file
