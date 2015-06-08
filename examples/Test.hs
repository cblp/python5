{-
    Python5 — a hypothetic language
    Copyright (C) 2015 - Yuriy Syrovetskiy

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE NamedFieldPuns #-}

import Control.Arrow      ( (>>>) )
import Control.Monad      ( forM_ )
import Data.List          ( delete, isSuffixOf )
import System.Directory   ( getCurrentDirectory, getDirectoryContents )
import System.Environment ( getEnvironment )
import System.FilePath    ( (</>) )
import System.Process     ( CreateProcess(env)
                          , proc
                          , readCreateProcess
                          )
import Test.Hspec

examplesDir :: String
examplesDir = "examples"

expectedOutput :: [(String, String)]
expectedOutput =
    [ ("calc.hs", "0.5\n8\n5.666666666666667\n5\n")
    , ("control.hs", "The product is: 384\n")
    , ("data.hs", unlines [ "[BANANA, APPLE, LIME]"
                          , "[(0, Banana), (1, Apple), (2, Lime)]" ] )
    , ("functions.hs", "0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 \n")
    , ("io.hs", "Hello, I'm Python5!\nWhat is your name?\nHi, TEST INPUT.\n")
    , ("types.hs", "ValueError ()\n")
    ]

main :: IO ()
main = do
    examples <- ( filter (".hs" `isSuffixOf`)
                  >>> delete "Test.hs" )
                <$> getDirectoryContents examplesDir
    hspec $
        describe "examples" $
            forM_ examples $ \ex ->
                it ex $ do
                    result <- python5 (examplesDir </> ex) "TEST INPUT"
                    Just result `shouldBe` lookup ex expectedOutput

python5 :: String -> String -> IO String
python5 scriptFile stdinContent = do
    cwd <- getCurrentDirectory
    curEnv <- getEnvironment
    let cmd = cwd </> "bin" </> "python5"
        args = [scriptFile]
        env = Just $ curEnv ++ [("PYTHON5_LOCALTEST", "1")]
        processInfo = (proc cmd args){env}
    readCreateProcess processInfo stdinContent
