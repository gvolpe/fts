module UI.Themes where

import           Control.Lens
import           Monomer
import qualified Monomer.Lens                  as L

customLightTheme :: Theme
customLightTheme =
  lightTheme & L.userColorMap . at "rowBgColor" ?~ rgbHex "#ECECEC"

customDarkTheme :: Theme
customDarkTheme =
  darkTheme & L.userColorMap . at "rowBgColor" ?~ rgbHex "#656565"
