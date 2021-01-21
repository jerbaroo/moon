{-# LANGUAGE OverloadedStrings #-}

module Moon.CLI where

import qualified Data.Text   as Text
import           Moon.Pretty  ( pretty )
import           Moon.Types   ( Image, Machine, Provider, System, image,
                                machine, provider, system, ec2 )

testImage :: Image
testImage = image "app" "jerbaroo/bridge-sim" "" ""

testProvider :: Provider
testProvider = provider ec2 $ [ machine "main" [testImage, testImage]]

testSystem :: System
testSystem = system "prod" [testProvider, testProvider]

runCLI :: IO ()
runCLI = putStrLn $ Text.unpack $ pretty testSystem
