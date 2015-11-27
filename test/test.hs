import           Disorder.Core.Main

import qualified Test.Anatomy.System.XmlDiff

main :: IO ()
main =
  disorderMain [
      Test.Anatomy.System.XmlDiff.tests
    ]
