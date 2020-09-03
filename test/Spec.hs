import Test.Tasty (defaultMain, testGroup)

import qualified Spec.Blogger as Blogger
import qualified Spec.OutputFormat as OutputFormat
import qualified Spec.Wordpress as Wordpress

main :: IO ()
main = defaultMain $ testGroup "Spec tests"
  [ Blogger.tests
  , Wordpress.tests
  , OutputFormat.tests
  ]
