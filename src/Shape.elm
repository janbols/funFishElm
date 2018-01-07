module Shape exposing (..)

import Vector exposing (Vector)

type Shape = Circle {center:Vector , radius:Vector}
    | Line {start: Vector, end: Vector}
    | Path {start: Vector, beziers: List Bezier}
    | Curve {start: Vector, bezier: Bezier}
    | Polygon {ps: List Vector}
type alias Bezier = {cp1: Vector, cp2: Vector, ep: Vector}
