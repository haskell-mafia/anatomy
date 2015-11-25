{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_anatomy

import           Anatomy.Ci.GitHub
import qualified Anatomy.Ci.Jenkins as J

import           P

import           Data.List ((++), stripPrefix)
import           Data.String

import           System.Exit
import           System.Environment
import           System.IO

main :: IO ()
main = sanity >> getArgs >>= \args -> case args of
  "list" : org : [] ->
    auth >>= \a -> list a org
  "stats" : org : [] ->
    auth >>= \a -> stats a org
  "hook" : org : project : [] ->
    auth >>= \a -> hook a org project
  "hooks": org : [] ->
    auth >>= \a -> hooks a org
  "create-job" : username : job : project : templatefile : [] ->
    auth' >>= \a -> J.createJob $ J.ModJob (J.Job username a job) templatefile (toLookup project Nothing)
  "update-job" : username : job : project : templatefile : [] ->
    auth' >>= \a -> J.updateJob $ J.ModJob (J.Job username a job) templatefile (toLookup project Nothing)
  "sync-jobs" : username : templatefile : [] -> do
    let prefix = "bin/ci"
    oauth <- auth'
    a <- auth
    repos' <- repos a username
    forM_ repos'  $ \project -> do
      ciFiles <- files a username project (prefix ++ "*")
      forM_ ciFiles $ \file -> do
        let jobName = project ++ (fromMaybe "" . stripPrefix prefix $ file)
        putStrLn ("Syncing " ++ jobName)
        J.createOrUpdateJob $ J.ModJob (J.Job username oauth jobName) templatefile (toLookup project (Just file))
  "get-job" : username : project : [] ->
    auth' >>= \a -> J.getJob $ J.Job username a project
  "--version" : [] ->
    putStrLn $ "version: " ++ buildInfoVersion
  _ ->
    usage
  where
    toLookup project script s = case s of
      "project"         -> Just project
      "env_CI_COMMAND"  -> script
      _                 -> Nothing

usage :: IO ()
usage =
  usage' >> exitFailure

usage' :: IO ()
usage' =
  putStrLn $ unlines [
      "usage: ci list [github user or org]"
    , "  e.g: ci list ambiata"
    , ""
    , "usage: ci stats [github user or org]"
    , "  e.g: ci stats ambiata"
    , ""
    , "usage: ci hook [github user or org] [project]"
    , "  e.g: ci hook ambiata ci"
    , ""
    , "usage: ci hooks [github org]"
    , "  e.g: ci hooks ambiata"
    , ""
    , "usage: ci create-job [jenkins user] [job] [project] [template]"
    , "  e.g: ci create-job markhibberd ci ci job-template.config.xml"
    , ""
    , "usage: ci update-job [jenkins user] [job] [project] [template]"
    , "  e.g: ci update-job markhibberd ci ci job-template.config.xml"
    , ""
    , "usage: ci get-job [jenkins user] [project]"
    , "  e.g: ci get-job markhibberd ci"
    , ""
    , "usage: ci sync-jobs [jenkins user] [template]"
    , "  e.g: ci sync-jobs markhibberd job-template.config.xml"
    , ""
    , "usage: ci --version"
    ]

sanity :: IO ()
sanity = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
