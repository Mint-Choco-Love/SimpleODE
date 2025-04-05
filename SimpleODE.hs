module SimpleODE where
import Text.Printf

data MethodType = Euler | AB2 | Dahlquist | RK4 | PECE2
    deriving (Eq, Show)

data IVP = IVP1D {
    derivative :: TValue -> XValue -> XValue,
    xis :: [Float],
    xi's :: [Float],
    step_size :: Float,
    from :: Float,
    to :: Float
} | IVP2D {
    derivative :: TValue -> XValue -> XValue,
    derivative2d :: TValue -> XValue -> YValue -> XValue,
    xis :: [Float],
    xi's :: [Float],
    step_size :: Float,
    from :: Float,
    to :: Float
} | IVP3D {
    derivative :: TValue -> XValue -> XValue,
    derivative2d :: TValue -> XValue -> YValue -> XValue,
    derivative3 :: TValue -> XValue -> YValue -> ZValue -> ZValue,
    xis :: [Float],
    xi's :: [Float],
    step_size :: Float,
    from :: Float,
    to :: Float
}

instance Show IVP where
    show (IVP1D _ xis xi's step_size from to) = show [show xis, show xi's, show step_size, show from, show to]
    show (IVP2D _ _ xis xi's step_size from to) = show [show xis, show xi's, show step_size, show from, show to]
    show (IVP3D _ _ _ xis xi's step_size from to) = show [show xis, show xi's, show step_size, show from, show to]


type TValue = Float
type XValue = Float
type YValue = Float
type ZValue = Float

data DimValue = Dim1 {
    tValue :: TValue, xValue :: XValue
} | Dim2 {
    tValue :: TValue, xValue :: XValue, yValue :: YValue
} | Dim3 {
    tValue :: TValue, xValue :: XValue, yValue :: YValue, zValue :: ZValue
}

instance Show DimValue where
    show (Dim1 t x) = printf "%.5f" t ++ ", " ++ printf "%.5f" x
    show (Dim2 t x y) = printf "%.5f" t ++ ", " ++ printf "%.5f" x ++ ", " ++ printf "%.5f" y
    show (Dim3 t x y z) = printf "%.5f" t ++ ", " ++ printf "%.5f" x ++ ", " ++ printf "%.5f" y ++ ", " ++ printf "%.5f" z