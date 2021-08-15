{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.TitleTextSpec where

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
  length s' + x === f (g s)
 where
  str = Gen.filter (/= "") $ Gen.string (Range.linear 0 10) Gen.lower
  gen = unwords <$> Gen.list (Range.linear 0 10) str
  f   = \(TitleText t) -> T.length t
  g s = fromString s :: TitleText

titleTextSpec :: Group
titleTextSpec = $$(discover)
