import qualified Command.ApkSpec (runTests)
import qualified Command.AptGetSpec (runTests)
import qualified Command.GemSpec (runTests)
import qualified Command.NpmSpec (runTests)
import qualified Command.PipSpec (runTests)
import qualified Command.WgetSpec (runTests)
import qualified PrintShellASTIntegrationTest (runTests)

main :: IO ()
main = do
  putStrLn "Run Unit Tests"
  Command.AptGetSpec.runTests
  Command.PipSpec.runTests
  Command.NpmSpec.runTests
  Command.GemSpec.runTests
  Command.WgetSpec.runTests
  Command.ApkSpec.runTests
  putStrLn "Run shell AST integration test"
  PrintShellASTIntegrationTest.runTests True
