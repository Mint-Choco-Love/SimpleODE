import SimpleODE
import ODEExample
import Data.List (intercalate)
import Text.Printf

-- gnuplot
-- plot "ode1.txt" using 1:2 with linespoints
-- replot "ode2.txt" using 1:2 with linespoints
-- replot "ode3.txt" using 1:2 with linespoints

main :: IO ()
main = do
    let ret1 = solve Euler example_6_1
    let ret2 = solve RK4 example_6_1
    let ret3 = solve AB2 example_6_1
    let ret4 = solve PECE2 example_6_1
    put "ode1.txt" ret1
    put "ode2.txt" ret2
    put "ode3.txt" ret3
    put "ode4.txt" ret4
    return ()




put :: String -> [DimValue] -> IO ()
put filename xs = do
    putStrLn text
    putStrLn ""
    writeFile filename text

    where
        text = intercalate "\n" $ map show xs

