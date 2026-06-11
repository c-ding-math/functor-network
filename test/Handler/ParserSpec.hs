module Handler.ParserSpec where
    
import Test.Hspec
import System.Process.Typed
import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy.Char8 as BL8
    
spec :: Spec
spec = describe "Parser" $ do
    it "readProcess should throw an exception" $ do
        readProcess (proc "some-command" ["arg1"]) `shouldThrow` anyException