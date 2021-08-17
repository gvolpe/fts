{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.TitleTextSpec where

import           Data.Coerce                    ( coerce )
import           Data.List.Extra                ( trim )
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import           Domain.Movie                   ( TitleText(..) )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

-- TitleText fromString adds ampersand in between words and trims white spaces
prop_title_text_from_string :: Property
prop_title_text_from_string = property $ do
  s <- forAll gen
  let s'  = trim s
  let amp = length (" &" :: String)
  let x = if s' == "" then 0 else (length (words s') - 1) * amp
  length s' + x === f s
 where
  rng = Range.linear 0 10
  str = Gen.filter (/= "") $ Gen.string rng Gen.lower
  gen = unwords <$> Gen.list rng str
  f s = T.length $ coerce (fromString s :: TitleText)

titleTextSpec :: Group
titleTextSpec = $$(discover)
