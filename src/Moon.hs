{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Moon where

import qualified Data.Set           as Set
import           Data.Set            ( Set )
import           Data.Text           ( Text )
import           Text.Pretty.Simple as Pretty

-- | Base implementation of 'Cloud'.
data Cloud' = Cloud'
  { name     :: Text
  , machines :: Set Machine
  } deriving (Eq, Show)

-- | Base implementation of 'Machine'.
data Machine' = Machine'
  { name  :: Text
  , nodes :: Set Node
  } deriving (Eq, Show)

-- | Base implementation of 'Node'.
data Node' = Node'
  { name     :: Text
  , repo     :: Text
  , start    :: Text
  , stop     :: Text
  } deriving (Eq, Show)

-- | Machines running on a single cloud.
--
-- Uniquely identified by name.
newtype Cloud = Cloud (Cloud')
  deriving Show

instance Eq Cloud where
  (Cloud a) == (Cloud b) = name (a :: Cloud') == name (b :: Cloud')

-- | Nodes running on a single machine.
--
-- Uniquely identified by name.
newtype Machine = Machine (Machine')
  deriving Show

instance Eq Machine where
  (Machine a) ==        (Machine b) = name (a :: Machine') ==        name (b :: Machine')
instance Ord Machine where
  (Machine a) `compare` (Machine b) = name (a :: Machine') `compare` name (b :: Machine')

-- | An application running in a Docker container.
--
-- Uniquely identified by name.
newtype Node = Node (Node')
  deriving Show

instance Eq Node where
  (Node a) ==        (Node b) = name (a :: Node') ==        name (b :: Node')
instance Ord Node where
  (Node a) `compare` (Node b) = name (a :: Node') `compare` name (b :: Node')

-- | Multiple 'Cloud's.
type Network = Set Cloud

-- | A 'Cloud' with given 'Machine's.
cloud :: Cloud' -> Set Machine -> Cloud
cloud p is = Cloud $ p { machines = is }

-- | A 'Machine' with given name and 'Node's.
machine :: Text -> Set Node -> Machine
machine n ns = Machine $ Machine' { name = n, nodes = ns }

-- | An application image from Docker Hub.
node :: Text -> Text -> Text -> Text -> Node
node n r start stop = Node $ Node'
  { name = n
  , repo = r
  , start = start
  , stop = stop
  }

-- | Amazon Elastic Cloud service provider.
ec2 :: Cloud'
ec2 = Cloud' { name = "EC2", machines = Set.empty }

--------------------------------------------------------------------------------

testCloud :: Cloud
testCloud = cloud ec2 $ Set.fromList
  [ machine "main" $ Set.fromList [ node "app" "jerbaroo/bridge-sim" "" ""]]

runCLI :: IO ()
runCLI = Pretty.pPrintOpt Pretty.CheckColorTty Pretty.defaultOutputOptionsDarkBg
  { outputOptionsIndentAmount  = 2
  , outputOptionsCompact       = True
  , outputOptionsCompactParens = True
  }
  testCloud
