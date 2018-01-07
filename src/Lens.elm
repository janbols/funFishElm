module Lens exposing (..)

import Box exposing (Box)

type Hue = Blackish | Greyish | Whiteish | Redish | Brownish | Beigish | Hollow

type alias Lens = {hue: Hue, box: Box}

rehue: Lens -> Lens
rehue l = {l| hue = case l.hue of
     Blackish -> Greyish
     Greyish -> Whiteish
     Whiteish -> Blackish

     Redish-> Brownish
     Brownish-> Beigish
     Beigish -> Redish

     Hollow -> Hollow
  }

