import SimpleODE
import Data.List (intercalate)
import Text.Printf

-- gnuplot
-- plot "ode1.txt" using 1:2 with linespoints
-- replot "ode12txt" using 1:2 with linespoints
main :: IO ()
main = do
    let ret1 = solve example_4_1
    let ret2 = solve example_4_1_euler
    put "ode1.txt" ret1
    put "ode2.txt" ret2
    return ()




put :: String -> [(Float, Float)] -> IO ()
put filename xs = do
    putStrLn text
    putStrLn ""
    writeFile filename text

    where
        text = intercalate "\n" $ map (\a -> printf "%.5f" (fst a) ++ " " ++ printf "%.5f" (snd a)) xs


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