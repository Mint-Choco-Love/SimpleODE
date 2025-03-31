module ODEExample where

import SimpleODE


example_2_1 = IVP1D {
    derivative = \t x -> (1 - 2 * t) * x,
    xis = [1],
    xi's = [1],
    step_size = 0.3,
    from = 0.3,
    to = 5.0
}

example_2_2 = IVP1D {
    derivative = \t x -> 2 * x * (1-x),
    xis = [0.2],
    xi's = [0.32],
    step_size = 0.2,
    from = 10.2,
    to = 15.0
}

example_4_1 = IVP1D {
    derivative = \t x -> (1 - 2 * t) * x,
    xis = [1, 1.1],
    xi's = [1, 0.88],
    step_size = 0.1,
    from = 0.2,
    to = 4.0
}

example_6_1 :: IVP
example_6_1 = IVP1D {
    derivative = \t x -> -8 * (x - 5 - 15 * exp (-t / 8)),
    xis = [100, 100],
    xi's = [-880, -880],
    step_size = 0.1,
    from = 0.1,
    to = 5
}


example_6_12 :: IVP 
example_6_12 = IVP1D {
    derivative = \t x -> -(8 * x),
    xis = [1],
    xi's = [-8],
    step_size = 1 / 120,
    from = 1 / 120,
    to = 3.0
}


