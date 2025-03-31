module SimpleODE where

data MethodType = Euler | AB2 | Dahlquist | RK4 | PECE2
    deriving (Eq, Show)

data IVP = IVP {
    derivative :: TValue -> XValue -> XValue,
    xis :: [Float],
    xi's :: [Float],
    step_size :: Float,
    from :: Float,
    to :: Float
}

instance Show IVP where
    show (IVP m xis xi's step_size from to) = show [show xis, show xi's, show step_size, show from, show to]

type TValue = Float
type XValue = Float
type YValue = Float
type ZValue = Float

type Dim1 = (TValue, XValue)
type Dim2 = (TValue, XValue, YValue)
type Dim3 = (TValue, XValue, YValue, ZValue)

solve :: MethodType -> IVP -> [Dim1]
solve methodType ivp
    | methodType == Euler = zip (iterate (+h) (t - h)) (xis ivp) ++ euler ivp
    | methodType == AB2 = zip (iterate (+h) (t - h * 2)) (xis ivp) ++ ab2 ivp
    | methodType == Dahlquist = zip (iterate (+h) (t - h * 2)) (xis ivp) ++ dahlquist ivp
    | methodType == RK4 = zip (iterate (+h) (t - h)) (xis ivp) ++ rk4 ivp
    | methodType == PECE2 = zip (iterate (+h) (t - h * 2)) (xis ivp) ++ pece2 ivp
    | otherwise = []
    where
        t = from ivp
        h = step_size ivp


euler :: IVP -> [Dim1]
euler ivp
    | t > tf = []
    | otherwise = (t, x1) : euler IVP {
        derivative = f,
        xis = [x1],
        xi's = [x1'],
        step_size = h,
        from = t + h,
        to = tf
    }

    where
        t = from ivp
        tf = to ivp
        h = step_size ivp
        f = derivative ivp
        x0 = head $ xis ivp
        x0' = head $ xi's ivp
        x1 = x0 + h * x0'
        x1' = f t x1
    
ab2 :: IVP -> [Dim1]
ab2 ivp
    | t > tf = []
    | otherwise = (t, x2) : ab2 IVP {
        derivative = f,
        xis = [x1, x2],
        xi's = [x1', x2'],
        step_size = h,
        from = t + h,
        to = tf
    }
    
    where
        t = from ivp
        tf = to ivp
        h = step_size ivp
        f = derivative ivp
        (x0:x1:_) = xis ivp
        (x0':x1':_) = xi's ivp
        x2 = x1 + 0.5 * h * (3 * x1' - x0')
        x2' = f t x2

-- completely worthless
dahlquist :: IVP -> [Dim1]
dahlquist ivp 
    | t > tf = []
    | otherwise = (t, x2) : dahlquist IVP {
        derivative = f,
        xis = [x1, x2],
        xi's = [x1', x2'],
        step_size = h,
        from = t + h,
        to = tf
    }

    where
        t = from ivp
        tf = to ivp
        h = step_size ivp
        f = derivative ivp
        (x0:x1:_) = xis ivp
        (x0':x1':_) = xi's ivp
        x2 = -(4 * x1) + 5 * x0 + h * (4 * x1' + 2 * x0')
        x2' = f t x2

rk4 :: IVP -> [Dim1]
rk4 ivp 
    | t > tf = []
    | otherwise = (t, x1) : rk4 IVP {
        derivative = f,
        xis = [x1],
        xi's = [],
        step_size = h,
        from = t + h,
        to = tf
    }

    where
        t = from ivp
        tf = to ivp
        h = step_size ivp
        f = derivative ivp
        (x0:_) = xis ivp
        
        k1 = f (t - h) x0
        k2 = f (t - 0.5 * h) (x0 + 0.5 * h * k1)
        k3 = f (t - 0.5 * h) (x0 + 0.5 * h * k2)
        k4 = f t (x0 + h * k3)

        x1 = x0 + h * (k1 / 6 + k2 / 3 + k3 /3 + k4 / 6)

pece2 :: IVP -> [Dim1]
pece2 ivp
    | t > tf = []
    | otherwise = (t, x2) : pece2 IVP {
        derivative = f,
        xis = [x1, x2],
        xi's = [x1', x2'],
        step_size = h,
        from = t + h,
        to = tf
    }

    where
        t = from ivp
        tf = to ivp
        h = step_size ivp
        f = derivative ivp
        (x0:x1:_) = xis ivp
        (x0':x1':_) = xi's ivp

        p = x1 + 0.5 * (3 * x1' - x0')
        e1 = f t p
        c = x1 - h / 12 * (5 * e1 + 8 * x1' - x0')
        e2 = f t c

        milne = -0.5 * (c - p)

        x2 = c + milne
        x2' = f t x2
