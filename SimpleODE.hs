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
    derivative2d :: TValue -> XValue -> YValue -> (XValue, YValue),
    xis :: [Float],
    xi's :: [Float],
    step_size :: Float,
    from :: Float,
    to :: Float
} | IVP3D {
    derivative3d :: TValue -> XValue -> YValue -> ZValue -> (XValue, YValue, ZValue),
    xis :: [Float],
    xi's :: [Float],
    step_size :: Float,
    from :: Float,
    to :: Float
}

instance Show IVP where
    show (IVP1D m xis xi's step_size from to) = show [show xis, show xi's, show step_size, show from, show to]
    show (IVP2D m xis xi's step_size from to) = show [show xis, show xi's, show step_size, show from, show to]
    show (IVP3D m xis xi's step_size from to) = show [show xis, show xi's, show step_size, show from, show to]


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

solve :: MethodType -> IVP -> [DimValue]
solve methodType (IVP1D f xis xi's h t tf)
    | methodType == Euler
        = map (\x -> Dim1 {tValue = fst x, xValue = snd x}) (zip (iterate (+h) (t - h)) xis) ++ euler (IVP1D f xis xi's h t tf)
    | methodType == AB2
        = map (\x -> Dim1 {tValue = fst x, xValue = snd x}) (zip (iterate (+h) (t - h * 2)) xis) ++ ab2 (IVP1D f xis xi's h t tf)
    | methodType == Dahlquist
        = map (\x -> Dim1 {tValue = fst x, xValue = snd x}) (zip (iterate (+h) (t - h * 2)) xis) ++ dahlquist (IVP1D f xis xi's h t tf)
    | methodType == RK4
        = map (\x -> Dim1 {tValue = fst x, xValue = snd x}) (zip (iterate (+h) (t - h )) xis) ++ rk4 (IVP1D f xis xi's h t tf)
    | methodType == PECE2
        = map (\x -> Dim1 {tValue = fst x, xValue = snd x}) (zip (iterate (+h) (t - h * 2)) xis) ++ pece2 (IVP1D f xis xi's h t tf)
    | otherwise = []


euler :: IVP -> [DimValue]
euler (IVP1D f xis xi's h t tf)
    | t > tf = []
    | otherwise = ret : euler IVP1D {
        derivative = f,
        xis = [x1],
        xi's = [x1'],
        step_size = h,
        from = t + h,
        to = tf
    }
    where
        (x0:_) = xis
        (x0':_) = xi's
        x1 = x0 + h * x0'
        x1' = f t x1

        ret = Dim1 {tValue = t, xValue = x1}
    
ab2 :: IVP -> [DimValue]
ab2 (IVP1D f xis xi's h t tf)
    | t > tf = []
    | otherwise = ret : ab2 IVP1D {
        derivative = f,
        xis = [x1, x2],
        xi's = [x1', x2'],
        step_size = h,
        from = t + h,
        to = tf
    }
    where
        (x0:x1:_) = xis
        (x0':x1':_) = xi's
        x2 = x1 + 0.5 * h * (3 * x1' - x0')
        x2' = f t x2

        ret = Dim1 {tValue = t, xValue = x2}

-- completely worthless
dahlquist :: IVP -> [DimValue]
dahlquist (IVP1D f xis xi's h t tf) 
    | t > tf = []
    | otherwise = ret : dahlquist IVP1D {
        derivative = f,
        xis = [x1, x2],
        xi's = [x1', x2'],
        step_size = h,
        from = t + h,
        to = tf
    }

    where
        (x0:x1:_) = xis
        (x0':x1':_) = xi's
        x2 = -(4 * x1) + 5 * x0 + h * (4 * x1' + 2 * x0')
        x2' = f t x2

        ret = Dim1 {tValue = t, xValue = x2}

rk4 :: IVP -> [DimValue]
rk4 (IVP1D f xis xi's h t tf)  
    | t > tf = []
    | otherwise = ret : rk4 IVP1D {
        derivative = f,
        xis = [x1],
        xi's = [],
        step_size = h,
        from = t + h,
        to = tf
    }

    where
        (x0:_) = xis
        
        k1 = f (t - h) x0
        k2 = f (t - 0.5 * h) (x0 + 0.5 * h * k1)
        k3 = f (t - 0.5 * h) (x0 + 0.5 * h * k2)
        k4 = f t (x0 + h * k3)

        x1 = x0 + h * (k1 / 6 + k2 / 3 + k3 /3 + k4 / 6)

        ret = Dim1 {tValue = t, xValue = x1}

pece2 :: IVP -> [DimValue]
pece2 (IVP1D f xis xi's h t tf) 
    | t > tf = []
    | otherwise = ret : pece2 IVP1D {
        derivative = f,
        xis = [x1, x2],
        xi's = [x1', x2'],
        step_size = h,
        from = t + h,
        to = tf
    }

    where
        (x0:x1:_) = xis
        (x0':x1':_) = xi's

        p = x1 + 0.5 * h * (3 * x1' - x0')
        e1 = f t p
        c = x1 - h / 12 * (5 * e1 + 8 * x1' - x0')
        e2 = f t c

        milne = -0.5 * (c - p)

        x2 = c + milne
        x2' = f t x2

        ret = Dim1 {tValue = t, xValue = x2}