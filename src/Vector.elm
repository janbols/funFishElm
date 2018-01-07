module Vector exposing (..)

type alias Vector = (Float, Float)
size : Vector -> Float
size (x,y) = sqrt <| x*x + y*y

add : Vector -> Vector-> Vector
add (x1,y1) (x2,y2)= (x1+x2, y1+y2)

neg : Vector -> Vector
neg (x,y) = (-x, -y)

sub : Vector -> Vector-> Vector
sub v1 v2= add v1 (neg v2)

mul : Vector -> Float-> Vector
mul (x,y) m= (x*m, y*m)

div : Vector -> Float -> Vector
div (x,y) d = (x / d, y / d)

zero: Vector
zero = (0.0, 0.0)