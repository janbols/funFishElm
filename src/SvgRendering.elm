module SvgRendering exposing (toSvg)

import Svg exposing (Svg)
import Svg.Attributes as SvgAtt
import Maybe exposing (Maybe(Just,Nothing))
import Vector exposing (..)
import Box exposing (Box)
import Shape exposing (..)
import Style exposing (..)



toSvg: Int -> (Shape, Style) -> Svg msg
toSvg height (shape, style) =
    let appliedStyle = (toSvgStroke style.stroke) ++ (toSvgFill style.fill)

        adjustVector : Vector -> Vector
        adjustVector (x,y) = (x, (toFloat height) - y)

        vectorToString: Vector -> String
        vectorToString (x,y) = toString(round x) ++ "," ++ toString(round y)

        adjustedVectorToString: Vector -> String
        adjustedVectorToString = vectorToString << adjustVector

        bezierToString: Bezier -> String
        bezierToString {cp1, cp2, ep} = String.join " " (List.map adjustedVectorToString [cp1, cp2, ep])

    in case shape of
    Polygon{ps} ->
        Svg.polygon ([
            SvgAtt.points (String.join " " (List.map adjustedVectorToString ps))
        ] ++ appliedStyle) []

    Curve{start, bezier} ->
        Svg.path ([
            SvgAtt.d ("M" ++ adjustedVectorToString start ++ " C" ++ bezierToString bezier )
        ] ++ appliedStyle) []

    Path{start, beziers} ->
        let bsCoords = String.join " C" (List.map bezierToString beziers)
        in Svg.path ([
            SvgAtt.d ("M" ++ adjustedVectorToString start ++ " C" ++ bsCoords )
        ] ++ appliedStyle) []

    Line{start, end} ->
        let (sx, sy) = adjustVector start
            (ex, ey) = adjustVector end
        in Svg.line ([
            SvgAtt.x1 (toString sx),
            SvgAtt.y1 (toString sy),
            SvgAtt.x2 (toString ex),
            SvgAtt.y2 (toString ey)
        ] ++ appliedStyle) []

    Circle{center, radius} ->
        let (ctrx, ctry) = adjustVector center
        in Svg.circle ([
            SvgAtt.cx (toString ctrx),
            SvgAtt.cy (toString ctry),
            SvgAtt.r  (toString <| size radius)
        ] ++ appliedStyle) []





toSvgStroke: Maybe StrokeStyle -> List (Svg.Attribute msg)
toSvgStroke it = case it of
    Just {width, color} -> [SvgAtt.strokeWidth (toString width), SvgAtt.stroke (toSvgColor color)]
    Nothing -> []


toSvgFill: Maybe FillStyle -> List (Svg.Attribute msg)
toSvgFill it = case it of
    Just {color} -> [SvgAtt.fill (toSvgColor color)]
    Nothing -> [SvgAtt.fill "transparent"]

toSvgColor: StyleColor -> String
toSvgColor color = case color of
    Black -> "black"
    Grey -> "grey"
    White -> "white"
    Red -> "orangeRed"
    Brown -> "tan"
    Beige -> "beige"
    Green -> "green"
    Yellow -> "yellow"