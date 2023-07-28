module PrintShellASTIntegrationTest where

import Control.Monad (filterM)
import Data.Bool (Bool (False))
import Data.List (intercalate)
import PrintShellAST (parseShell, printShellText, printToken)
import System.Directory (doesFileExist, getDirectoryContents)
import ToJSON (tokenToJSONString)

testFileDirecory :: String
testFileDirecory = "./test/test_shell_scripts/"

type ShowError = Bool

runTests :: ShowError -> IO ()
runTests showError = do
  dirContent <- getDirectoryContents testFileDirecory
  files <- filterM doesFileExist $ map (testFileDirecory ++) dirContent
  results <- mapM runBashTest files
  mapM_ (showResult showError) $ zip files results

showResult :: ShowError -> (FilePath, Bool) -> IO ()
showResult _ (file, True) = do
  putStrLn $ file ++ " ✔️"
showResult False (file, False) = do
  putStrLn $ file ++ " ❌"
showResult True (file, False) = do
  script <- readFile file
  let (Just originalAST) = parseShell script
  let generatedCode = printShellText printToken (Just originalAST)
  putStrLn $ file ++ " ❌"
  putStrLn "\n======== Generated Code ========"
  putStrLn generatedCode
  putStrLn "\n======== Original AST ========"
  putStrLn $ tokenToJSONString originalAST
  let (Just generatedAST) = parseShell generatedCode
  putStrLn "\n======== Generated AST ========"
  putStrLn $ tokenToJSONString generatedAST

runBashTest :: String -> IO Bool
runBashTest file = do
  content <- readFile file
  return $ runBashParseAndPrintTest content

-- Check that the printing of the Shell AST works for a specific bash script.
runBashParseAndPrintTest :: String -> Bool
runBashParseAndPrintTest script = originalAST == generatedAST
  where
    originalAST = parseShell script
    generatedCode = printShellText printToken originalAST
    generatedAST = parseShell generatedCode
