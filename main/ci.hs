{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_anatomy

import           Anatomy.Ci.GitHub
import           Anatomy.Ci.Jenkins (JenkinsUrl (..), HooksUrl (..))
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
    auth >>= \a ->
      list a org
  "stats" : org : [] ->
    auth >>= \a ->
      stats a org
  "hook" : org : project : [] ->
    auth >>= \a ->
      hook hooksA a org project
  "hook" : org : project : h : [] ->
    auth >>= \a ->
      hook (HooksUrl h) a org project
  "hooks": org : [] ->
    auth >>= \a ->
      hooks hooksA a org
  "hooks": org : h : [] ->
    auth >>= \a ->
      hooks (HooksUrl h) a org
  "create-job" : username : job : project : templatefile : [] ->
    auth' >>= \a ->
      J.createJob $ J.ModJob (J.Job username a job jenkinsA hooksA) templatefile (toLookup project Nothing)
  "create-job" : username : job : project : templatefile : j : h : [] ->
    auth' >>= \a ->
      J.createJob $ J.ModJob (J.Job username a job (JenkinsUrl j) (HooksUrl h)) templatefile (toLookup project Nothing)
  "update-job" : username : job : project : templatefile : [] ->
    auth' >>= \a ->
      J.updateJob $ J.ModJob (J.Job username a job jenkinsA hooksA) templatefile (toLookup project Nothing)
  "update-job" : username : job : project : templatefile : j : h :[] ->
    auth' >>= \a ->
      J.updateJob $ J.ModJob (J.Job username a job (JenkinsUrl j) (HooksUrl h)) templatefile (toLookup project Nothing)
  "sync-jobs" : username : templatefile : [] ->
    sync' username templatefile jenkinsA hooksA
  "sync-jobs" : username : templatefile : j : h : [] ->
    sync' username templatefile (JenkinsUrl j) (HooksUrl h)
  "get-job" : username : project : [] ->
    auth' >>= \a ->
      J.getJob $ J.Job username a project jenkinsA hooksA
  "--version" : [] ->
    putStrLn $ "version: " ++ buildInfoVersion
  _ ->
    usage


toLookup :: a -> Maybe a -> String -> Maybe a
toLookup project script s =
  case s of
    "project" ->
      Just project
    "env_CI_COMMAND" ->
      script
    _ ->
      Nothing

sync' :: String -> String -> JenkinsUrl -> HooksUrl -> IO ()
sync' username templatefile j h = do
  let prefix = "bin/ci"
  oauth <- auth'
  a <- auth
  repos' <- repos a username
  forM_ repos'  $ \project -> do
    ciFiles <- files a username project (prefix ++ "*")
    forM_ ciFiles $ \file -> do
      let jobName = project ++ (fromMaybe "" . stripPrefix prefix $ file)
      putStrLn ("Syncing " ++ jobName)
      J.createOrUpdateJob $ J.ModJob (J.Job username oauth jobName j h) templatefile (toLookup project (Just file))

jenkinsA :: JenkinsUrl
jenkinsA =
  JenkinsUrl "https://ci.ambiata.com"

hooksA :: HooksUrl
hooksA =
  HooksUrl "https://ci.ambiata.com"

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
    , "usage: ci hook [github user or org] [project] (hooks url)"
    , "  e.g: ci hook ambiata ci"
    , "  e.g: ci hook ambiata ci https://ci.ambiata.com"
    , ""
    , "usage: ci hooks [github org] (hooks url)"
    , "  e.g: ci hooks ambiata"
    , "  e.g: ci hooks ambiata https://ci.ambiata.com"
    , ""
    , "usage: ci create-job [jenkins user] [job] [project] [template] (jenkins url) (hooks url)"
    , "  e.g: ci create-job markhibberd ci ci job-template.config.xml"
    , "  e.g: ci create-job markhibberd ci ci job-template.config.xml https://ci.ambiata.com https://ci.ambiata.com"
    , ""
    , "usage: ci update-job [jenkins user] [job] [project] [template] (jenkins url) (hooks url)"
    , "  e.g: ci update-job markhibberd ci ci job-template.config.xml"
    , "  e.g: ci update-job markhibberd ci ci job-template.config.xml https://ci.ambiata.com https://ci.ambiata.com"
    , ""
    , "usage: ci get-job [jenkins user] [project]"
    , "  e.g: ci get-job markhibberd ci"
    , ""
    , "usage: ci sync-jobs [jenkins user] [template] (jenkins url) (hooks url)"
    , "  e.g: ci sync-jobs markhibberd job-template.config.xml"
    , "  e.g: ci sync-jobs markhibberd job-template.config.xml https://ci.ambiata.com https://ci.ambiata.com"
    , ""
    , "usage: ci --version"
    ]

sanity :: IO ()
sanity = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
