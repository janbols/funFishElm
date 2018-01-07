module Picture exposing (Picture,
    turn, flip, toss, besideRatio, beside, aboveRatio, above, over,
    ttile, utile, quartet, nonet, egg, eggband, squareLimit,
    createPicture, mapper, mapShape, getStrokeWidth)

import Vector exposing(..)
import Shape exposing(..)
import Style exposing(..)
import Box exposing(Box)

type alias Picture = Box -> List (Shape, Style)

turn: Picture -> Picture
turn p = Box.turn >> p

flip: Picture -> Picture
flip p = Box.flip >> p

toss: Picture -> Picture
toss p = Box.toss >> p

besideRatio: (Int, Int) -> (Picture, Picture) -> Picture
besideRatio (m, n) (p1, p2) = \box ->
    let factor = toFloat m / toFloat (m + n)
        (b1, b2) = Box.splitVertically factor box
    in p1 b1 ++ p2(b2)

beside: (Picture, Picture) -> Picture
beside (p1, p2) = besideRatio (1, 1) (p1, p2)

aboveRatio: (Int, Int) -> (Picture, Picture) -> Picture
aboveRatio (m, n) (p1, p2) = \ box ->
    let factor = toFloat m / toFloat (m + n)
        (b1, b2) = Box.splitHorizontally factor box
    in p1 b1 ++ p2 b2

above: (Picture, Picture) -> Picture
above (p1, p2) = aboveRatio (1, 1) (p1, p2)

over: (Picture, Picture) -> Picture
over (p1, p2) = \box -> p1 box  ++ p2 box




ttile: Picture -> Picture
ttile p = 
    let fishN = p |> toss |> flip
        fishE = fishN |> turn |> turn |> turn
    in over (p, over(fishN, fishE))

utile: Picture -> Picture
utile p = 
    let fishN = p |> toss |> flip
        fishW = fishN |> turn
        fishS = fishW |> turn
        fishE = fishS |> turn
    in over (over (fishN, fishW),  over (fishE,fishS))

quartet: (Picture,Picture,Picture,Picture) -> Picture
quartet (p, q, r, s) = above (beside (p, q), beside (r, s))

blank: Picture
blank = \box -> []

side: Int -> Picture -> Picture
side n p = 
    let s = if n == 1 then blank else side (n - 1) p
        t = ttile p
    in quartet (s, s, t |> turn, t)

corner: Int -> Picture -> Picture
corner n p = 
    let (c, s) = if n == 1  then (blank, blank) 
                            else (corner (n - 1) p, side (n - 1) p)
        u = utile p
    in quartet (c, s, s |> turn, u)

nonet: (Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture, Picture) -> Picture
nonet (p, q, r, s, t, u, v, w, x) =
    let
        firstRow: Picture
        firstRow = besideRatio (1, 2) (p, beside (q, r))
        secondRow: Picture
        secondRow = besideRatio (1, 2) (s, beside (t, u))
        thirdRow: Picture
        thirdRow = besideRatio (1, 2) (v, beside (w, x))
    in aboveRatio (1, 2) (firstRow, aboveRatio (1, 1) (secondRow, thirdRow))

bandify: ((Int, Int) -> (Picture, Picture) -> Picture) -> Int -> (Picture,Picture,Picture) -> Picture
bandify combineRatio n (first, middle, last) =
    let pictures = [first] ++ List.repeat (n - 2) middle
        operation: Picture -> (Picture, Int) -> (Picture, Int)
        operation item (accP, iP) = (combineRatio (1, iP) (item, accP), iP + 1)
        (result, _) = List.foldr operation (last, 1) pictures
    in result

aboveBand: Int -> (Picture,Picture,Picture) -> Picture
aboveBand n ps = bandify aboveRatio n ps

besideBand: Int -> (Picture,Picture,Picture) -> Picture
besideBand n ps = bandify besideRatio n ps

egg: (Int, Int) -> Picture -> Picture
egg (n, m) p =
    let sideN = side n p
        sideS = sideN |> turn |> turn
        center = utile p
        topband = besideBand m (sideN, sideN, sideN)
        midband = besideBand m (center, center, center)
        botband = besideBand m (sideS, sideS, sideS)
    in aboveBand n (topband, midband, botband)

eggband: Int -> Picture -> Picture
eggband n p =
    let theSide = side n p
        q = theSide
        t = p |> utile
        w = theSide |> turn |> turn
    in nonet(q, q, q, t, t, t, w, w, w)

squareLimit: Int -> Picture -> Picture
squareLimit n p =
    let cornerNW = corner n p
        cornerSW = turn cornerNW
        cornerSE = turn cornerSW
        cornerNE = turn cornerSE
        sideN = side n p
        sideW = turn sideN
        sideS = turn sideW
        sideE = turn sideS
        center = utile p
    in nonet(cornerNW, sideN, cornerNE,
             sideW, center, sideE,
             cornerSW, sideS, cornerSE)








createPicture: List Shape -> Picture
createPicture shapes box =
    let m = mapper box
        style = getStyle box
        mappedShapes = List.map (mapShape m) shapes
    in List.map (\s -> (s, style)) mappedShapes


mapper: Box -> Vector ->Vector
mapper box (x,y) = add box.a (add (mul box.b x) (mul box.c y))

mapShape: (Vector -> Vector) -> Shape -> Shape
mapShape m  s =
    let
        mapBezier: (Vector -> Vector) -> Bezier -> Bezier
        mapBezier m {cp1, cp2, ep} = Bezier (m cp1) (m cp2) (m ep)
    in case s of
    Polygon {ps} -> Polygon {ps= List.map m ps}
    Curve {start, bezier} -> Curve {start =m start, bezier =mapBezier m bezier}
    Path {start, beziers} -> Path {start=m start, beziers=List.map (mapBezier m) beziers}
    Line {start, end} -> Line {start=m start, end=m end}
    Circle{center, radius} -> Circle {center=m center, radius=sub (m (add center radius)) (m center)}

getStrokeWidth: Box -> Float
getStrokeWidth  box = (min (size box.b) (size box.c)) / 80

getStyle: Box -> Style
getStyle box = Style (Just (StrokeStyle (getStrokeWidth box) Black))  Nothing
