module Box exposing (..)

import Vector exposing (..)

type alias Box = {a: Vector, b: Vector, c: Vector}

turn: Box -> Box
turn {a,b,c} = Box (add a b) c (neg b)

flip: Box -> Box
flip {a,b,c} = Box (add a  b) (neg b) c

toss: Box -> Box
toss {a,b,c} = Box (add a (div (add b c) 2.0)) (div (add b c) 2.0)  (div (sub c b) 2.0)

scaleHorizontally: Float -> Box -> Box
scaleHorizontally s {a,b,c} = Box a (mul b s) c

scaleVertically: Float -> Box -> Box
scaleVertically s {a,b,c} = Box a b (mul c s)

moveHorizontally: Float -> Box -> Box
moveHorizontally offset {a,b,c} = Box (add a (mul b offset)) b c

moveVertically: Float -> Box -> Box
moveVertically offset {a,b,c} = Box (add a (mul c offset)) b c

splitHorizontally: Float -> Box -> (Box, Box)
splitHorizontally f box =
    let top = box |> moveVertically(1.0 - f) |> scaleVertically(f)
        bottom = box |> scaleVertically(1.0 - f)
    in (top, bottom)


splitVertically: Float -> Box -> (Box, Box)
splitVertically f box =
    let left = box |> scaleHorizontally(f)
        right = box |> moveHorizontally(f) |> scaleHorizontally(1.0 - f)
    in (left, right)

