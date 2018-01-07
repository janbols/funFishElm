module LensPicture exposing (LensPicture, 
    turn, flip, toss, besideRatio, beside, aboveRatio, above, over,
    ttile, utile, quartet, quartet2, nonet, egg, squareLimit,
    createLensPicture)

import Shape exposing(..)
import Style exposing(..)
import Lens exposing(..)
import Picture exposing(Picture)
import Maybe exposing(Maybe(Just,Nothing))


type alias LensPicture = Lens -> List (Shape, Style)


toPicture: Lens -> LensPicture -> Picture
toPicture lens p = \b -> {lens|box = b} |> p

lift: (Picture -> Picture) -> LensPicture -> LensPicture
lift f lp = \lens ->
    let toP = toPicture lens
    in lens.box |> (lp |> toP |> f)

lift2: ((Picture, Picture) -> Picture) -> (LensPicture, LensPicture) -> LensPicture
lift2 f (lp1, lp2) = \lens ->
    let toP = toPicture lens 
    in lens.box |> f (toP lp1 , toP lp2)

turn: LensPicture -> LensPicture
turn p = lift Picture.turn p

flip: LensPicture -> LensPicture
flip p = lift Picture.flip p

toss: LensPicture -> LensPicture
toss p = lift Picture.toss p

besideRatio: (Int, Int) -> (LensPicture, LensPicture) -> LensPicture
besideRatio (m, n) (p1, p2) = lift2 (Picture.besideRatio (m, n)) (p1, p2)

beside: (LensPicture, LensPicture) -> LensPicture
beside (p1, p2) = besideRatio (1, 1) (p1, p2)

aboveRatio: (Int, Int) -> (LensPicture, LensPicture) -> LensPicture
aboveRatio (m, n) (p1, p2) = lift2 (Picture.aboveRatio (m, n)) (p1, p2)

above: (LensPicture, LensPicture) -> LensPicture
above (p1, p2) = aboveRatio (1, 1) (p1, p2)

over: (LensPicture, LensPicture) -> LensPicture 
over (p1, p2) = lift2 Picture.over (p1, p2)

rehue: LensPicture -> LensPicture
rehue p = \lens -> lens |> Lens.rehue |> p





type alias LensTransformation = LensPicture -> LensPicture

ttile: LensTransformation -> LensTransformation -> LensPicture -> LensPicture
ttile hueN hueE f =
    let fishN = f |> toss |> flip
        fishE = fishN |> turn |> turn |> turn
    in over (f, over(fishN |> hueN, fishE |> hueE))

ttile0: LensPicture -> LensPicture
ttile0 f = ttile identity identity f

ttile1: LensPicture -> LensPicture
ttile1 f = ttile rehue (rehue >> rehue) f

ttile2: LensPicture -> LensPicture
ttile2 f = ttile (rehue >> rehue) rehue f

utile: LensTransformation -> LensTransformation -> LensTransformation -> LensTransformation -> LensPicture -> LensPicture
utile hueN hueW hueS hueE f =
    let fishN = f |> (toss >> flip)
        fishW = fishN |> turn
        fishS = fishW |> turn
        fishE = fishS |> turn
    in over (
      over (fishN |> hueN, fishW |> hueW),
      over (fishE |> hueE, fishS |> hueS)
    )

utile0: LensPicture -> LensPicture
utile0 f = utile identity identity identity identity f

utile1: LensPicture -> LensPicture
utile1 f = utile (rehue >> rehue) identity (rehue >> rehue) identity f

utile2: LensPicture -> LensPicture
utile2 f = utile identity (rehue >> rehue) rehue (rehue >> rehue) f

utile3: LensPicture -> LensPicture
utile3 f = utile (rehue >> rehue) identity rehue identity f

quartet: (LensPicture, LensPicture, LensPicture, LensPicture) -> LensPicture
quartet (p, q, r, s) = above (beside(p, q), beside(r, s))


quartet2: Int -> LensPicture -> LensPicture
quartet2 depth p =
    let
        qquartet: Int -> LensPicture -> LensPicture
        qquartet n p =
            let p2 = if n == 1 then p else qquartet (n - 1) p
            in quartet (p2, p2, p2, p2)

        pNW = p
        pNE = p |> rehue |> turn
        pSW = p |> rehue |> turn |> turn |> turn
        pSE = p |> turn |> turn
        q = quartet(pNW, pNE, pSW, pSE)
    in qquartet(depth)( q)

blank: LensPicture
blank = \_ -> []

side: LensTransformation -> LensTransformation -> LensTransformation -> Int -> LensPicture -> LensPicture
side tt hueSW hueSE n p =
    let s = if n == 1 then blank else side tt hueSW hueSE (n - 1) p
        t = tt p
    in quartet (s, s, t |> turn |> hueSW, t |> hueSE)

side0: Int -> LensPicture -> LensPicture
side0 n p = side ttile0 identity identity n p

side1:Int -> LensPicture -> LensPicture
side1 n p = side ttile1 identity rehue n p

side2:Int -> LensPicture -> LensPicture
side2 n p = side ttile2 (rehue >> rehue) rehue n p

corner: LensTransformation -> (Int -> LensPicture -> LensPicture) -> (Int -> LensPicture -> LensPicture) -> Int -> LensPicture -> LensPicture
corner ut sideNE sideSW n p =
    let (c, ne, sw) = if (n == 1) then (blank, blank, blank)
                                  else (corner ut sideNE sideSW (n - 1) p, sideNE (n - 1) p, sideSW (n - 1) p)
        u = ut p
    in quartet (c, ne, sw |> turn, u)


corner1: Int -> LensPicture -> LensPicture
corner1 n p = corner utile3 side1 side2 n p

corner2: Int -> LensPicture -> LensPicture
corner2 n p = corner utile2 side2 side1 n p

nonet: (LensPicture, LensPicture, LensPicture, LensPicture, LensPicture, LensPicture, LensPicture, LensPicture, LensPicture) -> LensPicture
nonet (p, q, r, s, t, u, v, w, x) =
    aboveRatio(1, 2)(besideRatio(1, 2)(p, beside(q, r)),
      aboveRatio(1, 1)(besideRatio(1, 2)(s, beside(t, u)),
        besideRatio(1, 2)(v, beside(w, x))))

squareLimit: Int -> LensPicture -> LensPicture
squareLimit n p =
    let cornerNW = corner1 n p
        cornerSW = (corner2 n p) |> turn
        cornerSE = cornerNW |> turn |> turn
        cornerNE = cornerSW |> turn |> turn
        sideN = side1 n p
        sideW = (side2 n p) |> turn
        sideS = sideN |> turn |> turn
        sideE = sideW |> turn |> turn
        center = utile1 p
    in nonet (
      cornerNW, sideN, cornerNE,
      sideW, center, sideE,
      cornerSW, sideS, cornerSE
    )

bandify: ((Int, Int) -> (LensPicture, LensPicture) -> LensPicture) -> Int -> (LensPicture, LensPicture,LensPicture) -> LensPicture
bandify combineRatio n (first, middle, last) =
    let pictures = [first] ++ List.repeat (n - 2) middle
        operation: LensPicture -> (LensPicture, Int) -> (LensPicture, Int)
        operation item (accP, iP) = (combineRatio (1, iP) (item, accP), iP + 1)
        (result, _) = List.foldr operation (last, 1) pictures
    in result


aboveBand: Int -> (LensPicture, LensPicture,LensPicture) -> LensPicture
aboveBand n (first, middle, last) = bandify aboveRatio n (first, middle, last)

besideBand: Int -> (LensPicture, LensPicture,LensPicture) -> LensPicture
besideBand n (first, middle, last) = bandify besideRatio n (first, middle, last)


egg: (Int, Int) -> LensPicture -> LensPicture
egg (n, m) p =
    let sideN = side0 n p
        sideS = sideN |> turn |> turn
        center = utile0 p
        topband = besideBand m (sideN, sideN, sideN)
        midband = besideBand m (center, center, center)
        botband = besideBand m (sideS, sideS, sideS)
    in aboveBand 3 (topband, midband, botband)







createLensPicture: List (String, Shape) -> LensPicture
createLensPicture shapes = \lens ->
    List.map (mapNamedShape lens) shapes


getDefaultColor: String -> Hue -> StyleColor
getDefaultColor name hue =
    if "secondary" == name then
      case hue of 
        Blackish -> White
        Greyish -> White
        Whiteish -> Black

        Redish -> Beige
        Brownish -> Beige
        Beigish -> Red

        Hollow -> Black
    else 
      case hue of 
        Blackish -> Black
        Greyish -> Grey
        Whiteish -> White

        Redish -> Red
        Brownish -> Brown
        Beigish -> Beige

        Hollow -> White

getDefaultStyle: String -> Float -> Hue -> Style
getDefaultStyle name sw hue = {stroke= Just {width=sw, color= getDefaultColor name hue}, fill=Nothing}

getCircleStyle: String -> Float -> Hue -> Style
getCircleStyle name sw hue = {stroke=Nothing, fill= Just {color=getDefaultColor name hue}}

isInnerEye name = String.endsWith "-inner" name

isOuterEye name = String.endsWith "-outer" name

getPathFillColor: String -> Hue -> StyleColor 
getPathFillColor name hue = case hue of 
    Blackish ->
        if name == "primary" then Black
        else if isOuterEye name then White
        else if isInnerEye name then Black
        else White
    Greyish ->
        if name == "primary"  then Grey
        else if isOuterEye name then White
        else if isInnerEye name then Grey
        else White
    Whiteish ->
        if name == "primary"  then White
        else if isOuterEye name then White
        else if isInnerEye name then Black
        else Black

    Redish ->
        if name == "primary"  then Red
        else if isOuterEye name then Beige
        else if isInnerEye name then Red
        else White
    Brownish ->
        if name == "primary"  then Brown
        else if isOuterEye name then Beige
        else if isInnerEye name then Brown
        else White
    Beigish ->
        if name == "primary"  then Beige
        else if isOuterEye name then Beige
        else if isInnerEye name then Red
        else Red

    Hollow -> White


getPathStyle: String -> Float -> Hue -> Style
getPathStyle name sw hue = case hue of
    Hollow -> {
      stroke= Just {width= sw, color= Black},
      fill= if isInnerEye name then Just {color=Black} else Nothing }
    _ -> {
      stroke = if isOuterEye name then Just {width=sw, color=getDefaultColor "secondary" hue} else Nothing,
      fill = Just {color= getPathFillColor name hue} }


mapNamedShape: Lens -> (String, Shape) -> (Shape, Style) 
mapNamedShape {box, hue} (name, shape) = 
    let m = Picture.mapper box 
        sw = Picture.getStrokeWidth box
    in (Picture.mapShape m shape, case shape of
       Path _-> getPathStyle name sw hue
       Circle _ -> getCircleStyle name sw hue
       _ -> getDefaultStyle name sw hue
    )





