{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anatomy.System.XmlDiff (
    XmlDiff (..)
  , xmlDiffText
  , xmlDiff
  , xmlDiffNode
  , elementsPath
  ) where

import           Anatomy.Data

import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import           P

import           Text.XML


data XmlDiff =
  XmlDiff [Element] (Node, Node)
  deriving (Eq, Show)

xmlDiffText :: Text -> Text -> Either XmlDiffError (Either XmlDiff ())
xmlDiffText t1 t2 = do
   let parse = first (XmlParseError . T.pack . show) . parseText def . TL.fromStrict
   xmlDiff
     <$> parse t1
     <*> parse t2

xmlDiff :: Document -> Document -> Either XmlDiff ()
xmlDiff d1 d2 =
  xmlDiffNode (NodeElement $ documentRoot d1) (NodeElement $ documentRoot d2)

-- Compares two xml nodes.
-- NOTE: Currently only works if they are identical, both in ordering and whitespace (for now).
xmlDiffNode :: Node -> Node -> Either XmlDiff ()
xmlDiffNode n1 n2 =
  let diff = Left $ XmlDiff [] (n1, n2)
  in case (n1, n2) of
    (NodeInstruction _, NodeInstruction _) ->
      diff
    (NodeComment _, NodeComment _) ->
      pure ()
    (NodeElement e1, NodeElement e2) ->
      if elementName e1 == elementName e2
        then
          first (\(XmlDiff e n) -> XmlDiff (e1 : e) n) $
            -- If we need better error reporting or edge case handling pull in `these`
            if (length $ elementNodes e1) /= (length $ elementNodes e2)
              then
                diff
              else
                traverse_ (uncurry xmlDiffNode) $ L.zip (elementNodes e1) (elementNodes e2)
        else diff
    (NodeContent t1, NodeContent t2) ->
      -- We may want to add a trim here if the diff gets noisy
      if T.strip t1 == T.strip t2
        then pure ()
        else diff
    _ ->
      diff

elementsPath :: [Element] -> Text
elementsPath e =
  "/" <> T.intercalate "/" (fmap (nameLocalName . elementName) e)
