module SimpleODE where

data MethodType = Euler | AB2 | Dahlquist
    deriving (Eq, Show)

data IVP = IVP {
    methodType :: MethodType,
    derivative :: Float -> Float -> Float,
    xis :: [Float],
    xi's :: [Float],
    step_size :: Float,
    from :: Float,
    to :: Float
}

instance Show IVP where
    show (IVP m _ xis xi's step_size from to) = show [show m, show xis, show xi's, show step_size, show from, show to]

solve :: IVP -> [(Float, Float)]
solve ivp
    | methodType ivp == Euler = zip (iterate (+h) (t - h)) (xis ivp) ++ euler ivp
    | methodType ivp == AB2 = zip (iterate (+h) (t - h * 2)) (xis ivp) ++ ab2 ivp
    | methodType ivp == Dahlquist = zip (iterate (+h) (t - h * 2)) (xis ivp) ++ dahlquist ivp
    | otherwise = []
    where
        t = from ivp
        h = step_size ivp


euler :: IVP -> [(Float, Float)]
euler ivp
    | t > tf = []
    | otherwise = (t, x1) : euler IVP {
        methodType = Euler,
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
    
ab2 :: IVP -> [(Float, Float)]
ab2 ivp
    | t > tf = []
    | otherwise = (t, x2) : ab2 IVP {
        methodType = AB2,
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
dahlquist :: IVP -> [(Float, Float)]
dahlquist ivp 
    | t > tf = []
    | otherwise = (t, x2) : dahlquist IVP {
        methodType = Dahlquist,
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