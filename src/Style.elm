module Style exposing (..)

type alias Style = {stroke: Maybe StrokeStyle, fill: Maybe FillStyle}
type alias StrokeStyle = {width: Float, color: StyleColor}
type alias FillStyle = {color: StyleColor}
type StyleColor = Black| Grey| White| Red| Brown| Beige| Green| Yellow

blackStrokeStyle = StrokeStyle 1.0 Black
