module Fishier exposing (fishShapes)

import Shape exposing (..)


fishyPath = Path {start=(0.000, 0.000), beziers=[
        Bezier (0.110, 0.110) (0.175, 0.175) (0.250, 0.250),
        Bezier (0.372, 0.194) (0.452, 0.132) (0.564, 0.032),
        Bezier (0.730, 0.056) (0.834, 0.042) (1.000, 0.000),
        Bezier (0.896, 0.062) (0.837, 0.107) (0.766, 0.202),
        Bezier (0.660, 0.208) (0.589, 0.217) (0.500, 0.250),
        Bezier (0.500, 0.410) (0.500, 0.460) (0.500, 0.500),
        Bezier (0.500, 0.575) (0.500, 0.625) (0.500, 0.750),
        Bezier (0.411, 0.783) (0.340, 0.792) (0.234, 0.798),
        Bezier (0.163, 0.893) (0.104, 0.938) (0.000, 1.000),
        Bezier (-0.042, 0.834) (-0.056, 0.730) (-0.032, 0.564),
        Bezier (-0.132, 0.452) (-0.194, 0.372) (-0.250, 0.250),
        Bezier (-0.150, 0.150) (-0.050, 0.050) (0.000, 0.000)
    ]}

leftEyePath = Path {start=(0.004, 0.800), beziers=[
        Bezier (0.040, 0.772) (0.068, 0.696) (0.074, 0.685),
        Bezier (0.045, 0.660) (0.010, 0.617) (-0.008, 0.592),
        Bezier (-0.017, 0.685) (-0.012, 0.770) (0.004, 0.800)
    ]}

innerLeftEyePath = Path {start=(0.018, 0.720), beziers=[
        Bezier (0.038, 0.708) (0.053, 0.684) (0.057, 0.674),
        Bezier (0.035, 0.652) (0.010, 0.622) (0.008, 0.618),
        Bezier (0.005, 0.685) (0.010, 0.700) (0.018, 0.720)
    ]}

rightEyePath = Path {start=(0.095, 0.870), beziers=[
        Bezier (0.160, 0.840) (0.200, 0.790) (0.205, 0.782),
        Bezier (0.165, 0.760) (0.140, 0.740) (0.115, 0.715),
        Bezier (0.095, 0.775) (0.090, 0.830) (0.095, 0.870)
    ]}


innerRightEyePath = Path {start=(0.128, 0.810), beziers=[
        Bezier (0.150, 0.805) (0.174, 0.783) (0.185, 0.774),
        Bezier (0.154, 0.756) (0.139, 0.740) (0.132, 0.736),
        Bezier (0.126, 0.760) (0.122, 0.795) (0.128, 0.810)
    ]}

fishySpineCurves = [
    Curve {start=(0.840, 0.070),bezier= Bezier (0.350, 0.120) (0.140, 0.500) (0.025, 0.900)}, --main spine
    Curve {start=(-0.015, 0.520),bezier= Bezier (0.040, 0.400) (0.120, 0.300) (0.210, 0.260)}, --left fin stem
    Curve {start=(0.475, 0.270),bezier= Bezier (0.320, 0.350) (0.340, 0.600) (0.240, 0.770)}, --right fin stem
    Curve {start=(0.377, 0.377),bezier= Bezier (0.410, 0.410) (0.460, 0.460) (0.495, 0.495)}, --right fin bottom delimiter
    Curve {start=(0.430, 0.165),bezier= Bezier (0.480, 0.175) (0.490, 0.220) (0.490, 0.230)}, --tail fin stem
    Curve {start=(0.452, 0.178),bezier= Bezier (0.510, 0.130) (0.540, 0.110) (0.600, 0.080)}, --tail fin bottom line
    Curve {start=(0.482, 0.215),bezier= Bezier (0.520, 0.200) (0.600, 0.160) (0.740, 0.150)}, --tail fin top line
    Curve {start=(-0.170, 0.237),bezier= Bezier (-0.125, 0.355) (-0.065, 0.405) (0.010, 0.480)}, --left fin top line
    Curve {start=(-0.110, 0.175),bezier= Bezier (-0.060, 0.250) (-0.030, 0.300) (0.080, 0.365)}, --left fin middle line
    Curve {start=(-0.045, 0.115),bezier= Bezier (0.010, 0.180) (0.060, 0.230) (0.170, 0.280)}, --left fin bottom line
    Curve {start=(0.270, 0.700),bezier= Bezier (0.340, 0.720) (0.426, 0.710) (0.474, 0.692)}, --right fin top line
    Curve {start=(0.310, 0.570),bezier= Bezier (0.400, 0.622) (0.435, 0.618) (0.474, 0.615)}, --right fin middle line
    Curve {start=(0.350, 0.435),bezier= Bezier (0.400, 0.505) (0.422, 0.520) (0.474, 0.538)} --right fin bottom line
  ]


fishShapes = [
    ("primary", fishyPath),
    ("eye-outer", leftEyePath),
    ("eye-outer", rightEyePath),
    ("eye-inner", innerLeftEyePath),
    ("eye-inner", innerRightEyePath)
  ] ++ (List.map (\s -> ("secondary", s)) fishySpineCurves)

