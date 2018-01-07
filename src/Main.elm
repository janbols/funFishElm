import Html exposing (Html)
import Html.Attributes as HtmlAtt
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Svg.Attributes as SvgAtt
import Maybe exposing (Maybe(Just,Nothing))
import Array exposing (Array)
import Vector exposing (..)
import Box exposing (Box)
import Shape exposing (..)
import Style exposing (..)
import Picture as P exposing (Picture, createPicture)
import Lens exposing (..)
import LensPicture as LP exposing (LensPicture, createLensPicture)
import Letter
import Fishy exposing (hendersonFishShapes)
import Fishier exposing (fishShapes)
import Lizard exposing (lizardShapes)

main =  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


fittedBox: (Int, Int) -> Box
fittedBox (w ,h) = Box (0.0, 0.0) (toFloat w, 0.0) (0.0, toFloat h)

expandedBox: (Int, Int) -> Box
expandedBox (w,h) = Box (toFloat w / 4.0, toFloat h / 4.0) (toFloat w / 2.0, 0.0) (0.0, toFloat h / 2.0)

bandBox: (Int, Int) -> Box
bandBox _ = Box (100.0, 100.0) (3200.0, 0.0) (0.0, 600.0)


type alias Model =
  { page: Int
  , title: String
  , dimensions: (Int,Int)
  , drawing: (Int, Int) -> List (Shape, Style)
  }

pages: Array (Int -> Model)
pages = Array.fromList [
        (\nr -> Model nr "basic" (800,800) (fittedBox >> (createPicture [Letter.f])))
        , (\nr -> Model nr "nonet" (800,800) (fittedBox >> (P.nonet (createPicture [Letter.h],
                                                                    createPicture [Letter.e],
                                                                    createPicture [Letter.n],
                                                                    createPicture [Letter.d1, Letter.d2],
                                                                    createPicture [Letter.e],
                                                                    createPicture [Letter.r1, Letter.r2],
                                                                    createPicture [Letter.s],
                                                                    createPicture [Letter.o1, Letter.o2],
                                                                    createPicture [Letter.h] ))))
        , (\nr -> Model nr "fish" (800,800) (expandedBox >> (createPicture hendersonFishShapes)))
        , (\nr -> Model nr "ttile" (800,800) (expandedBox >> (P.ttile (createPicture hendersonFishShapes))))
        , (\nr -> Model nr "eggBand" (1200,800) (bandBox >> (P.egg (3,16) (createPicture hendersonFishShapes))))
        , (\nr -> Model nr "square limit" (800,800) (fittedBox >> (P.squareLimit 3 (createPicture hendersonFishShapes))))
        , (\nr -> Model nr "black fish" (800,800) (expandedBox >> Lens Blackish >> (createLensPicture fishShapes)))
        , (\nr -> Model nr "grey fish" (800,800) (expandedBox >> Lens Greyish >> (createLensPicture fishShapes)))
        , (\nr -> Model nr "white fish" (800,800) (expandedBox >> Lens Whiteish >> (createLensPicture fishShapes)))
        , (\nr -> Model nr "lizard" (800,800) (expandedBox >> Lens Greyish >> (createLensPicture lizardShapes)))
        , (\nr -> Model nr "lizards" (800,800) (fittedBox >> Lens Blackish >> (LP.quartet2 3 (createLensPicture lizardShapes))))
        , (\nr -> Model nr "fishBand" (1200,800) (bandBox >> Lens Hollow >> (LP.egg (3, 16) (createLensPicture fishShapes))))
        , (\nr -> Model nr "square limit" (800,800) (fittedBox >> Lens Greyish >> (LP.squareLimit 3 (createLensPicture fishShapes))))
        , (\nr -> Model nr "square limit in color" (800,800) (fittedBox >> Lens Brownish >> (LP.squareLimit 3 (createLensPicture fishShapes))))
        ]

getModelFromPage: Int -> Maybe Model
getModelFromPage page =
    let ix = page - 1
    in Maybe.map (\model -> model page) (Array.get ix pages)

model : Maybe Model
model = getModelFromPage 1



-- UPDATE


type Msg
  = ToPage Int

update : Msg -> Maybe Model -> Maybe Model
update msg inputModel =
  case msg of
    ToPage newPage -> getModelFromPage newPage


-- VIEW


view : Maybe Model -> Html Msg
view maybeModel =
    case maybeModel of
        Just model ->
            let
                prevPageNr = model.page - 1
                nextPageNr = model.page + 1
                (w, h) = model.dimensions
                svgAttr = [
                    SvgAtt.width <| toString w
                    , SvgAtt.height <| toString h ]
                svgChildren = List.map (toSvg h) (model.drawing model.dimensions)
            in
                Html.div [HtmlAtt.id "content"] ([
                    css "index.css"
                    , Html.div [ HtmlAtt.id "canvas"] [Svg.svg svgAttr svgChildren]
                    , Html.div [ HtmlAtt.id "title"] [ Html.text <| "Page " ++ toString model.page ++ ": " ++ model.title ]
                    ]
                    ++ (if prevPageNr > 0 then [pageButton "prev" prevPageNr] else [])
                    ++ (if nextPageNr <= Array.length pages then [pageButton "next" nextPageNr] else [])
                )
        Nothing -> Html.div [] [Html.text "Nothing to render"]

css: String -> Html Msg
css url = Html.node "link" [ HtmlAtt.rel "stylesheet", HtmlAtt.href url ] []

pageButton: String -> Int -> Html Msg
pageButton idName nr =
    let page = "Page " ++ toString(nr)
    in Html.button [ HtmlAtt.id idName, onClick <| ToPage nr] [ Html.text page ]



toSvg: Int -> (Shape, Style) -> Svg Msg
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