import Navigation exposing (program, Location)
import Html exposing (Html)
import Html.Attributes as HtmlAtt
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Svg.Attributes
import SvgRendering
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


main = program (MoveToPage << pageFromLocation)
            { init = init
            , view = view
            , update = update
            , subscriptions = (\_ -> Sub.none)
            }




-- MODEL

type alias Model =
  { page: Int
  , title: String
  , dimensions: (Int,Int)
  , drawing: (Int, Int) -> List (Shape, Style)
  }


fittedBox: (Int, Int) -> Box
fittedBox (w ,h) = Box (0.0, 0.0) (toFloat w, 0.0) (0.0, toFloat h)

expandedBox: (Int, Int) -> Box
expandedBox (w,h) = Box (toFloat w / 4.0, toFloat h / 4.0) (toFloat w / 2.0, 0.0) (0.0, toFloat h / 2.0)

bandBox: (Int, Int) -> Box
bandBox _ = Box (100.0, 100.0) (3200.0, 0.0) (0.0, 600.0)

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

modelFromPage: Int -> Maybe Model
modelFromPage page =
    let ix = page - 1
    in Maybe.map (\model -> model page) (Array.get ix pages)

pageFromLocation: Location -> Int
pageFromLocation location =
    case String.toInt (String.dropLeft 1 location.hash) of
        Ok value -> value
        Err _ -> 1  --go to page 1 when in doubt

-- INIT

init : Navigation.Location -> ( Maybe Model, Cmd Msg )
init location =
  ( modelFromPage <| pageFromLocation location
  , Cmd.none
  )


-- UPDATE

type Msg
  = MoveToPage Int

update : Msg -> Maybe Model -> (Maybe Model, Cmd Msg)
update msg inputModel =
  case msg of
    MoveToPage newPage -> (modelFromPage newPage, Cmd.none)


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
                    Svg.Attributes.width <| toString w
                    , Svg.Attributes.height <| toString h ]
                svgChildren = List.map (SvgRendering.toSvg h) (model.drawing model.dimensions)
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
        link = "#" ++ toString(nr)
    in Html.a [ HtmlAtt.id idName, HtmlAtt.href link] [ Html.text page ]


