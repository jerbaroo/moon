{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Moon.Pretty where

import           Moon.Types  ( Image(..), Machine(..), Provider(..), System(..) )
import           Data.Text   ( Text )
import qualified Data.Text  as Text

data PrettyConfig = PrettyConfig
  { prettyImage    :: Image    -> Text
  , prettyMachine  :: Machine  -> Text
  , prettyProvider :: Provider -> Text
  , prettySystem   :: System   -> Text
  }

defaultPrettyConfig :: PrettyConfig
defaultPrettyConfig = PrettyConfig
  { prettyImage    = \image'    -> name (image'    :: Image   )
  , prettyMachine  = \machine'  -> name (machine'  :: Machine )
  , prettyProvider = \provider' -> name (provider' :: Provider)
  , prettySystem   = \system'   -> name (system'   :: System  )
  }

class Pretty a where
  prettyConfigF     :: PrettyConfig -> a -> Text
  pretty            :: a ->                                 Text
  prettyOpt         :: a ->                 PrettyConfig -> Text
  prettyOptPre      :: a -> Bool -> Text -> PrettyConfig -> Text
  prettyOptPreInner :: a ->         Text -> PrettyConfig -> [Text]
  pretty       a                        = prettyOpt a defaultPrettyConfig
  prettyOpt    a                        = prettyOptPre a True ""
  prettyOptPre a lastA pre prettyConfig = Text.intercalate "\n" $
    [pre <> "|--" <> (prettyConfigF prettyConfig) a]
    ++ prettyOptPreInner a (pre <> if lastA then "  " else "| ") prettyConfig

instance Pretty Image where
  prettyConfigF           = prettyImage
  prettyOptPreInner _ _ _ = []

instance Pretty Machine where
  prettyConfigF     = prettyMachine
  prettyOptPreInner = prettyOptPreInnerHelper . images

instance Pretty Provider where
  prettyConfigF     = prettyProvider
  prettyOptPreInner = prettyOptPreInnerHelper . machines

instance Pretty System where
  prettyConfigF     = prettySystem
  prettyOptPreInner = prettyOptPreInnerHelper . providers

prettyOptPreInnerHelper :: Pretty a => [a] -> Text -> PrettyConfig -> [Text]
prettyOptPreInnerHelper inners pre prettyConfig =
  map (\(b, i) -> prettyOptPre i b pre prettyConfig) $ bools inners

bools :: [a] -> [(Bool, a)]
bools []     = []
bools (a:[]) = [(True, a)]
bools (a:as) = (False, a) : bools as
