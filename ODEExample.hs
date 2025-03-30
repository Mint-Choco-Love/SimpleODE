module ODEExample where

import SimpleODE


example_2_1 = IVP {
    methodType = Euler,
    derivative = \t x -> (1 - 2 * t) * x,
    xis = [1],
    xi's = [1],
    step_size = 0.3,
    from = 0.3,
    to = 5.0
}

example_2_2 = IVP {
    methodType = Euler,
    derivative = \t x -> 2 * x * (1-x),
    xis = [0.2],
    xi's = [0.32],
    step_size = 0.2,
    from = 10.2,
    to = 15.0
}

example_4_1 = IVP {
    methodType = AB2,
    derivative = \t x -> (1 - 2 * t) * x,
    xis = [1, 1.1],
    xi's = [1, 0.88],
    step_size = 0.1,
    from = 0.2,
    to = 4.0
}

example_4_1_euler = IVP {
    methodType = Euler,
    derivative = \t x -> (1 - 2 * t) * x,
    xis = [1],
    xi's = [1],
    step_size = 0.1,
    from = 0.1,
    to = 4.0
}

example_4_1_rk4 :: IVP
example_4_1_rk4 = IVP {
    methodType = RK4,
    derivative = \t x -> (1 - 2 * t) * x,
    xis = [1],
    xi's = [1],
    step_size = 0.1,
    from = 0.1,
    to = 4.0
}

example_6_1_euler :: IVP
example_6_1_euler = IVP {
    methodType = Euler,
    derivative = \t x -> -(8 * x) - 40 * (3 * exp (-(t / 8)) - 1),
    xis = [100],
    xi's = [-880],
    step_size = 0.01,
    from = 0.01,
    to = 50
}

example_6_1_ab2 :: IVP
example_6_1_ab2 = IVP {
    methodType = AB2,
    derivative = \t x -> -(8 * x) - 40 * (3 * exp (-(t / 8)) - 1),
    xis = [100, 100],
    xi's = [-880, -880],
    step_size = 0.01,
    from = 0.01,
    to = 50
}

example_6_1_rk4 :: IVP
example_6_1_rk4 = IVP {
    methodType = RK4,
    derivative = \t x -> -(8 * x) - 40 * (3 * exp (-t / 8) - 1),
    xis = [100],
    xi's = [],
    step_size = 0.01,
    from = 0.01,
    to = 50
}

example :: IVP
example = IVP {
    methodType = RK4,
    derivative = \t x -> -8 * (x - 5 - 15 * exp (-t / 8)),
    xis = [100],
    xi's = [],
    step_size = 0.01,
    from = 0.01,
    to = 50
}

example_6_12_euler :: IVP 
example_6_12_euler = IVP {
    methodType = Euler,
    derivative = \t x -> -(8 * x),
    xis = [1],
    xi's = [-8],
    step_size = 1 / 120,
    from = 1 / 120,
    to = 3.0
}

example_6_12_rk4 :: IVP 
example_6_12_rk4 = IVP {
    methodType = RK4,
    derivative = \t x -> -(8 * x),
    xis = [1],
    xi's = [],
    step_size = 1 / 120,
    from = 1 / 120,
    to = 3.0
}

