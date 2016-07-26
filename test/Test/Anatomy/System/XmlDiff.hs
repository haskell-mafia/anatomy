{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Anatomy.System.XmlDiff where

import           Anatomy.System.XmlDiff

import           P

import           System.IO

import           Test.QuickCheck


prop_xmldiff_test_same =
  let xml = "<xml><foo>a</foo><bar>b</bar></xml>"
  in xmlDiffText xml xml === Right (Right ())

prop_xmldiff_test_diff =
  xmlDiffFailure "/xml/bar" $
    xmlDiffText "<xml><foo>a</foo><bar>b</bar></xml>" "<xml><foo>a</foo><bar>c</bar></xml>"

prop_xmldiff_test_diff_children_length =
  xmlDiffFailure "/xml" $
    xmlDiffText "<xml><foo>a</foo><bar>b</bar></xml>" "<xml><foo>a</foo></xml>"

prop_xmldiff_strip =
  xmlDiffText "<xml><foo> a</foo></xml>" "<xml><foo>a</foo></xml>" === Right (Right ())


xmlDiffFailure :: Text -> Either a (Either XmlDiff ()) -> Property
xmlDiffFailure p f =
  case f of
    Left _ -> counterexample "XML parse error" False
    Right (Right ()) -> counterexample "XML was the same" False
    Right (Left (XmlDiff e _)) -> elementsPath e === p

return []
tests :: IO Bool
tests = $quickCheckAll
