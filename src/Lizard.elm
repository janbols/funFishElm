module Lizard exposing (lizardShapes)

import Shape exposing (..)



lizardPath = Path{start=(0.000, 0.000), beziers=[
            Bezier (0.020, 0.050) (0.030, 0.120) (0.025, 0.185),
            Bezier (0.100, 0.120) (0.200, 0.085) (0.310, 0.090),
            Bezier (0.310, 0.000) (0.310, -0.100) (0.310, -0.313),
            Bezier (0.450, -0.170) (0.500, -0.100) (0.625, 0.070),
            Bezier (0.700, 0.040) (0.780, 0.010) (0.850, 0.000),
            Bezier (0.700, -0.070) (0.563, -0.180) (0.563, -0.313),
            Bezier (0.680, -0.310) (0.780, -0.410) (0.813, -0.375),
            Bezier (0.792, -0.333) (0.771, -0.292) (0.750, -0.250),
            Bezier (0.800, -0.200) (0.900, -0.100) (1.000, 0.000),
            Bezier (0.900, 0.100) (0.800, 0.200) (0.750, 0.250),
            Bezier (0.900, 0.650) (1.050, 0.750) (1.250, 0.850),
            Bezier (1.200, 0.940) (1.100, 0.980) (1.000, 1.000),
            Bezier (0.980, 0.900) (0.940, 0.800) (0.850, 0.750),
            Bezier (0.750, 0.950) (0.650, 1.100) (0.250, 1.250),
            Bezier (0.200, 1.200) (0.100, 1.100) (0.000, 1.000),
            Bezier (0.050, 0.950) (0.150, 0.850) (0.250, 0.750),
            Bezier (0.375, 0.813) (0.375, 0.813) (0.375, 0.813),
            Bezier (0.410, 0.780) (0.310, 0.680) (0.313, 0.563),
            Bezier (0.180, 0.563) (0.070, 0.700) (0.000, 0.850),
            Bezier (-0.010, 0.780) (-0.040, 0.700) (-0.070, 0.625),
            Bezier (0.100, 0.500) (0.170, 0.450) (0.313, 0.310),
            Bezier (0.100, 0.310) (0.000, 0.310) (-0.090, 0.310),
            Bezier (-0.085, 0.200) (-0.120, 0.100) (-0.185, 0.025),
            Bezier (-0.120, 0.030) (-0.050, 0.020) (0.000, 0.000)
        ]}

lizardEyeOuterCircles = [
    Circle {center=(0.260, 1.100), radius=(0.070, 0.0)},
    Circle {center=(0.260, 0.900), radius=(0.070, 0.0)}
  ]

lizardEyeInnerCircles = [
    Circle {center=(0.260, 1.100), radius=(0.050, 0.0)},
    Circle {center=(0.260, 0.900), radius=(0.050, 0.0)}
    ]

mainSpineCurves = [
    Curve {start= (0.350, -0.200), bezier = Bezier (0.700, 0.900) (0.650, 1.000) (0.075, 1.000)}
  ]

namedShapes: String -> List Shape -> List (String, Shape)
namedShapes name shapes = List.map (\s -> (name, s)) shapes

lizardShapes: List (String, Shape)
lizardShapes = [
    ("primary", lizardPath)
  ]
  ++ (namedShapes "secondary" mainSpineCurves)
  ++ (namedShapes "secondary" lizardEyeOuterCircles)
  ++ (namedShapes "primary" lizardEyeInnerCircles)
