{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Moon.Types where

import           Data.Text  ( Text )

-- | A provider of machines.
data Provider = Provider
  { name     :: Text
  , machines :: [Machine]
  } deriving (Eq, Show)

-- | A machine that runs Docker images.
data Machine = Machine
  { name   :: Text
  , images :: [Image]
  } deriving (Eq, Show)

-- | An application as a Docker image.
data Image = Image
  { name     :: Text
  , repo     :: Text
  , start    :: Text
  , stop     :: Text
  } deriving (Eq, Show)

-- | Images across potentially many providers and machines.
data System = System
  { name      :: Text
  , providers :: [Provider]
  }

-- | A 'System' with given name and 'Provider's.
system :: Text -> [Provider] -> System
system name providers = System {..}

-- | A 'Provider' with given 'Machine's.
provider :: Provider -> [Machine] -> Provider
provider provider machines = provider { machines }

-- | A 'Machine' with given name and 'Node's.
machine :: Text -> [Image] -> Machine
machine name images = Machine {..}

-- | An image from Docker Hub.
image :: Text -> Text -> Text -> Text -> Image
image name repo start stop = Image {..}

-- | Amazon Elastic Compute Cloud service provider.
ec2 :: Provider
ec2 = Provider { name = "EC2", machines = [] }
